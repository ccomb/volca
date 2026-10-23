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
    Provenance (..),
    changesReport,
) where

import Data.Foldable (asum)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Data.JournalFile (Entry (..))
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
    AllocationKey (..),
    BiosphereFlow (..),
    Database (..),
    ExchangeLocation (..),
    Parameter (..),
    ParameterDefinition (..),
    Release (..),
    StatedAmount (..),
    TechnosphereFlow (..),
    WasteFlow (..),
    allocationKeyText,
    getActivity,
    locationCode,
 )

-- | One change, said: when, why, what in a sentence, and each line it touched.
data HistoryLine = HistoryLine
    { hlAt :: Text
    , hlNote :: Maybe Note
    , hlSummary :: Text
    , hlDetails :: [Text]
    }
    deriving (Eq, Show)

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
         in HistoryLine (jeAt event) (changeNote (jeOp event)) summary details

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
    SetPrice flow (Just price) -> "Product " <> flowNamed namer flow <> ": price set to " <> number (saAmount price) <> " " <> saUnit price
    SetPrice flow Nothing -> "Product " <> flowNamed namer flow <> ": price removed"
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
    , pvReallocated :: Maybe AllocationKey
    }

{- | The report shipped beside a modified database's data: which original it
was made from, that its publisher did not make these changes, and every change
with its reason, oldest first.
-}
changesReport :: Provenance -> [HistoryLine] -> Text
changesReport provenance lines' =
    T.unlines $
        [ "# Changes to " <> pvDatabase provenance
        , ""
        , "Modified from " <> pvOriginal provenance <> maybe "" ((", " <>) . releaseTitle) (pvRelease provenance) <> "."
        , ""
        , "Modified data; not endorsed by the publisher of the original."
        ]
            <> maybe [] (\key -> ["", reallocated key]) (pvReallocated provenance)
            <> concatMap section lines'
  where
    reallocated key = case key of
        Unallocated -> "Multi-output processes are kept whole, not divided as the original divides them."
        _ -> "Multi-output processes are divided by " <> allocationKeyText key <> ", not as the original divides them."
    section line =
        ["", "## " <> hlAt line <> ": " <> hlSummary line, "", "Reason: " <> maybe "No reason given" noteText (hlNote line)]
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
