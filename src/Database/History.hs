{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A database's journal as its reader reads it: what each change did, in
words, with the reason its author gave. The same words make the history a
reader browses and the report shipped beside a modified database's data, so
the two never tell different stories.
-}
module Database.History (
    HistoryLine (..),
    Namer (..),
    namerFor,
    describeJournal,
    Carried (..),
    Journal (..),
    Provenance (..),
    changesReport,
) where

import Data.Foldable (asum)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Data.JournalFile (Author (..), Entry (..))
import Database.Author (
    ActivityText (..),
    AuthoredActivity (..),
    AuthoredExchange (..),
    ExchangeEdit (..),
    ExchangeSelector (..),
    FlowRef (..),
    resolveProcess,
 )
import Database.Journal (
    Change (..),
    EditLine (..),
    JournalEvent,
    JournalOp (..),
    Note,
    ParameterLine (..),
    Prior (..),
    noteText,
 )
import Types (
    Activity (..),
    BiosphereFlow (..),
    Database (..),
    ExchangeLocation (..),
    FileReading (..),
    ModelParameter (..),
    Parameter (..),
    ParameterDefinition (..),
    Reading (..),
    Release (..),
    StatedAmount (..),
    SystemModel (..),
    TechnosphereFlow (..),
    WasteFlow (..),
    getActivity,
    locationCode,
    propertyName,
 )

-- | One change, said: when, by whom, why, what in a sentence, and each line it touched.
data HistoryLine = HistoryLine
    { hlAt :: Text
    , hlBy :: Maybe Author
    , hlNote :: Maybe Note
    , hlSummary :: Text
    , hlDetails :: [Text]
    }
    deriving (Eq, Show)

{- | What a database's data carry, in the order it applies: the parent it is
divided from, replayed before the division, then the database itself. A child
keeps no copy of its parent's journal, yet its data hold every line of it.
-}
data Carried a = Carried
    { carriedFromParent :: Maybe a
    , carriedOwn :: a
    }
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | The lines of one journal, said, and the database they were made on.
data Journal a = Journal
    { journalOf :: a
    , journalLines :: [HistoryLine]
    }
    deriving (Eq, Show, Functor)

{- | How the history names what a change points at: by name where the database
still holds it, by identifier where it does not, as for an activity deleted
since.
-}
data Namer = Namer
    { activityNamed :: Text -> Text
    , flowNamed :: UUID -> Text
    }

namerFor :: Maybe Database -> Namer
namerFor = \case
    Nothing -> Namer quoted UUID.toText
    Just db ->
        Namer
            { activityNamed = \pid -> maybe (quoted pid) (\act -> titled (activityName act) (activityLocation act)) (resolveProcess db pid >>= getActivity db)
            , flowNamed = \flowId ->
                maybe (UUID.toText flowId) quoted $
                    asum
                        [ bfName <$> M.lookup flowId (dbBioFlows db)
                        , tfName <$> M.lookup flowId (dbTechFlows db)
                        , wfName <$> M.lookup flowId (dbWasteFlows db)
                        ]
            }

describeJournal :: Namer -> [JournalEvent] -> [HistoryLine]
describeJournal namer = map line
  where
    line event =
        let (summary, details) = describeOp namer (changeOp (jeOp event))
         in HistoryLine (jeAt event) (jeBy event) (changeNote (jeOp event)) summary details

describeOp :: Namer -> JournalOp -> (Text, [Text])
describeOp namer = \case
    Created activities pids ->
        ("Added " <> counted (length activities) "activity" "activities", zipWith (\act pid -> authoredTitle act <> ", " <> pid) activities pids)
    Replaced target act ->
        ("Rewrote " <> activityNamed namer target, ["Now " <> authoredTitle act <> ", with " <> counted (length (aaExchanges act)) "exchange" "exchanges"])
    Deleted pids ->
        ("Deleted " <> counted (length pids) "activity" "activities", map (activityNamed namer) pids)
    Edited target editLines ->
        ("Edited " <> activityNamed namer target, map (describeEdit namer) editLines)
    ParametersEdited parameterLines ->
        ("Changed " <> counted (length parameterLines) "database parameter" "database parameters", map describeParameterLine parameterLines)

describeEdit :: Namer -> EditLine -> Text
describeEdit namer (EditLine edit _ before) = case edit of
    SetAmount selector amount -> selected selector <> ": amount " <> fromPrior before <> "set to " <> number amount
    RemoveExchange selector -> selected selector <> ": removed" <> wasPrior before
    SetFormula selector (Just formula) -> selected selector <> ": formula set to " <> formula
    SetFormula selector Nothing -> selected selector <> ": formula removed"
    SetProperty prop flow (Just value) -> "Product " <> flowNamed namer flow <> ": " <> propertyName prop <> " set to " <> number (saAmount value) <> " " <> saUnit value
    SetProperty prop flow Nothing -> "Product " <> flowNamed namer flow <> ": " <> propertyName prop <> " removed"
    AddExchange exchange -> "Added " <> authoredExchange exchange
    SetText (ActivityName name) -> "Name set to " <> quoted name
    SetText (ActivityLocation location) -> "Location set to " <> location
    SetText (ActivityDescription _) -> "Description rewritten"
    SetParameter name parameter -> "Parameter " <> name <> " set to " <> parameterValue parameter
    RemoveParameter name -> "Parameter " <> name <> " removed"
  where
    selected :: ExchangeSelector -> Text
    selected = \case
        SelectInput provider -> "Input from " <> activityNamed namer provider
        SelectWaste provider -> "Waste output to " <> activityNamed namer provider
        SelectBiosphere flowId -> "Flow " <> flowNamed namer flowId
        SelectUnlinkedInput flowId location -> "Input of " <> flowNamed namer flowId <> at location <> " naming no provider"
        SelectUnlinkedWaste flowId location -> "Waste output of " <> flowNamed namer flowId <> at location <> " naming no treatment"
    at :: Maybe ExchangeLocation -> Text
    at = \case
        Nothing -> ""
        Just LocationNone -> " with no location"
        Just stated -> " in " <> locationCode stated
    authoredExchange :: AuthoredExchange -> Text
    authoredExchange = \case
        AuthoredTechInput{atiProvider = provider, atiAmount = amount} -> "input from " <> activityNamed namer provider <> ", " <> number amount
        AuthoredWasteOutput{awProvider = provider, awAmount = amount} -> "waste output to " <> activityNamed namer provider <> ", " <> number amount
        AuthoredBio{abFlow = FlowById flowId, abAmount = amount} -> "flow " <> flowNamed namer flowId <> ", " <> number amount
        AuthoredBio{abFlow = FlowByName name _ _, abAmount = amount} -> "flow " <> quoted name <> ", " <> number amount

-- | "from 2.0 " before the new value, when the line kept what it replaced.
fromPrior :: Prior [Double] -> Text
fromPrior = \case
    PriorUnknown -> ""
    Prior [] -> ""
    Prior amounts -> "from " <> T.intercalate ", " (map number amounts) <> " "

wasPrior :: Prior [Double] -> Text
wasPrior = \case
    PriorUnknown -> ""
    Prior [] -> ""
    Prior amounts -> " (was " <> T.intercalate ", " (map number amounts) <> ")"

describeParameterLine :: ParameterLine -> Text
describeParameterLine (ParameterLine name parameter before) = case (before, parameter) of
    (PriorUnknown, Just now) -> name <> " set to " <> parameterValue now
    (PriorUnknown, Nothing) -> name <> " removed"
    (Prior Nothing, Just now) -> name <> " added: " <> parameterValue now
    (Prior Nothing, Nothing) -> name <> " removed"
    (Prior (Just earlier), Just now) -> name <> " changed from " <> parameterValue earlier <> " to " <> parameterValue now
    (Prior (Just earlier), Nothing) -> name <> " removed (was " <> parameterValue earlier <> ")"

parameterValue :: Parameter -> Text
parameterValue parameter = value (parameterDefinition parameter) <> maybe "" (" " <>) (parameterUnit parameter)
  where
    value = \case
        InputParameter amount _ -> number amount
        CalculatedParameter formula _ -> "= " <> formula

{- | Where a modified database's data came from, as its report cites it: the
database whose files it reads, and the published release those files are,
when declared, with the division of its multi-output processes when it is not
theirs.
-}
data Provenance = Provenance
    { pvDatabase :: Text
    , pvOriginal :: Text
    , pvRelease :: Maybe Release
    , pvReallocated :: Maybe Reading
    }

{- | The report shipped beside a modified database's data: which original it
was made from, that its publisher did not make these changes, and every change
with its reason, oldest first.
-}
changesReport :: Provenance -> Carried (Journal Text) -> Text
changesReport provenance carried =
    T.unlines $
        [ "# Changes to " <> pvDatabase provenance
        , ""
        , "Modified from " <> pvOriginal provenance <> maybe "" ((", " <>) . releaseTitle) (pvRelease provenance) <> "."
        , ""
        , "Modified data; not endorsed by the publisher of the original."
        ]
            <> maybe [] (\key -> ["", reallocated key]) (pvReallocated provenance)
            <> foldMap fromParent (carriedFromParent carried)
            <> concatMap section (journalLines (carriedOwn carried))
  where
    fromParent (Journal _ []) = []
    fromParent (Journal parent lines') =
        ["", "Changes made on " <> parent <> ", the parent this database is divided from, which its data carry:"]
            <> concatMap section lines'
            <> (if null (journalLines (carriedOwn carried)) then [] else ["", "Changes made on this database:"])
    reallocated reading = case reading of
        FromFiles ReadWhole -> "Multi-output processes are kept whole, not divided as the original divides them."
        FromFiles ReadDeclared -> "Multi-output processes are divided by the shares the original declares."
        ByModel model ->
            "Multi-output processes are divided by the system model "
                <> smName model
                <> " "
                <> smVersion model
                <> ", not as the original divides them."
                <> (if null (smParameters model) then "" else " The model also sets parameters of the processes: " <> T.intercalate ", " (map mpName (smParameters model)) <> ".")
    section line =
        ["", "## " <> hlAt line <> ": " <> hlSummary line, ""]
            <> maybe [] (\by -> ["By: @" <> authorHandle by]) (hlBy line)
            <> ["Reason: " <> maybe "No reason given" noteText (hlNote line)]
            <> (if null (hlDetails line) then [] else "" : map ("- " <>) (hlDetails line))
    releaseTitle release = T.unwords ([releaseName release, releaseVersion release] <> maybe [] pure (releaseSystemModel release))

authoredTitle :: AuthoredActivity -> Text
authoredTitle act = titled (aaName act) (aaLocation act)

titled :: Text -> Text -> Text
titled name location = quoted name <> if T.null location then "" else " (" <> location <> ")"

quoted :: Text -> Text
quoted text = "\"" <> text <> "\""

counted :: Int -> Text -> Text -> Text
counted 1 one _ = "1 " <> one
counted n _ many = T.pack (show n) <> " " <> many

number :: Double -> Text
number = T.pack . show
