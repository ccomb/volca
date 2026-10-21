{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module API.Types.Authoring where

import API.JsonOptions (Stripped (..))
import Data.Aeson
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe, maybeToList)
import Data.OpenApi (ToSchema (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Database.Author (
    ActivityText (..),
    AuthoredActivity (..),
    AuthoredExchange (..),
    ExchangeEdit (..),
    ExchangeSelector (..),
    FlowRef (..),
 )
import GHC.Generics
import Types (
    BioDirection (..),
    Compartment (..),
    Parameter (..),
    ParameterDefinition (..),
    UUID,
    Uncertainty,
    locationCode,
    parseMedium,
    readExchangeLocation,
    unknownMedium,
 )

{- | Request for delete-by-selection. Two exclusive selection modes: the
filter fields select the whole matching set (pagination ignored), or @dsqIds@
names the set exactly – the filter fields must then be absent. @dsqKeep@
spares selected process ids and @dsqExtra@ adds ones the selection missed.
Process ids are the canonical @activityUUID_productUUID@ strings the UI/CLI
carry, not matrix indices.
-}

{- | One technosphere input: a product this activity consumes, named by the
process that supplies it.

@tiProvider@ is a @process_id@ (@activityUUID_productUUID@, or a bare activity
UUID when that activity has a single product) – the same currency the read
endpoints hand out. The flow follows from the supplier, so it is never stated
separately. @tiUnit@ defaults to the supplier's own reference unit; stating
another one is allowed as long as it converts.
-}
data TechInputAPI = TechInputAPI
    { tiProvider :: Text
    , tiAmount :: Double
    , tiUnit :: Maybe Text
    , tiComment :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped TechInputAPI)

{- | One biosphere exchange: a resource taken from the environment or an
emission released into it.

Either @beFlow@ names a flow by its identifier, or @beName@ with
@beCompartment@ and @beUnit@ names one in words, which reaches the flow the
vocabulary already declares under them and introduces one only when nothing
answers to them. Giving both is refused rather than guessed at. A biosphere
amount is never converted, so an exchange states its amount in the flow's own
unit.
-}
data BioExchangeAPI = BioExchangeAPI
    { beFlow :: Maybe Text
    , beName :: Maybe Text
    , beCompartment :: Maybe Text
    , beSubCompartment :: Maybe Text
    , beDirection :: Text
    -- ^ @resource@ (taken from the environment) or @emission@ (released into it)
    , beAmount :: Double
    , beUnit :: Maybe Text
    , beComment :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped BioExchangeAPI)

{- | One waste output: a residue this activity hands to a treatment process.
@woProvider@ names that treatment process, exactly as a technosphere input
names its producer.
-}
data WasteOutputAPI = WasteOutputAPI
    { woProvider :: Text
    , woAmount :: Double
    , woUnit :: Maybe Text
    , woComment :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped WasteOutputAPI)

{- | An activity as a client writes it.

The inventory arrives as three lists rather than one tagged list, so a field
that makes sense for a supplier link cannot be sent on an emission and back
again. One reference product per activity: coproducts and allocation are a
later phase, and this shape does not pretend to accept them.

Identity is not sent. It is minted from the name, location, product and unit
below, so writing the same activity twice addresses the same row instead of
creating a second one.
-}
data ActivityInput = ActivityInput
    { aiName :: Text
    , aiLocation :: Text
    , aiDescription :: [Text]
    , aiProductName :: Text
    , aiProductAmount :: Double
    , aiProductUnit :: Text
    , aiInputs :: [TechInputAPI]
    , aiBiosphere :: [BioExchangeAPI]
    , aiWasteOutputs :: [WasteOutputAPI]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivityInput)

-- | A batch of activities to write, and why, if the author says.
data ActivityWriteRequest = ActivityWriteRequest
    { awrActivities :: [ActivityInput]
    , awrNote :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivityWriteRequest)

{- | What a write produced: the process ids now addressable, whether they
survive an unload, and everything the engine wants the author to know but did
not refuse over.
-}
data ActivityWriteResponse = ActivityWriteResponse
    { awpWritten :: [Text]
    , awpTransient :: Bool
    {- ^ True when the write lives in memory only, because the database is one
    the engine reads from configuration rather than owns.
    -}
    , awpWarnings :: [Text]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ActivityWriteResponse)

{- | Which lines of an inventory an edit addresses.

@esKind@ is @input@, @waste@ or @biosphere@: the first two name their provider
by process id, the third names its flow by identity. An input or a waste output
that names no provider is named by its flow instead, and by its location when
@esLocation@ is given. The product side – the
reference product, coproducts, a treatment's reference input – has no kind
here, because changing those changes what the activity /is/, not what it
consumes.

A selector naming several lines applies to all of them, and the answer says
how many. Naming none is refused: an edit that quietly did nothing reads
exactly like one that worked.
-}
data ExchangeSelectorAPI = ExchangeSelectorAPI
    { esKind :: Text
    , esProvider :: Maybe Text
    , esFlow :: Maybe Text
    , esLocation :: Maybe Text
    -- ^ For an input or a waste output named by its flow: the location the line states, "" for none; absent, any location.
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ExchangeSelectorAPI)

-- | The lines to restate, and the amount to restate them to.
data SetAmountAPI = SetAmountAPI
    { saSelect :: ExchangeSelectorAPI
    , saAmount :: Double
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped SetAmountAPI)

-- | The lines to give a formula, and the formula, in VoLCA's language; none drops it and keeps the amount.
data SetFormulaAPI = SetFormulaAPI
    { sfSelect :: ExchangeSelectorAPI
    , sfFormula :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped SetFormulaAPI)

{- | A parameter of the activity to add, or to replace under the same spelling:
a value, or a formula in VoLCA's language whose value the engine computes.
-}
data SetParameterAPI = SetParameterAPI
    { spName :: Text
    , spValue :: Maybe Double
    , spFormula :: Maybe Text
    , spUnit :: Maybe Text
    , spDescription :: Maybe Text
    , spUncertainty :: Maybe Uncertainty -- An input parameter's only
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped SetParameterAPI)

{- | Changes to one activity: lines to drop, lines to restate, lines to add,
the texts to restate, and its parameters.

Lists rather than one tagged list, for the reason 'ActivityInput' has
three: what makes sense on an addition cannot be sent on a removal and back
again. They apply in one order: removals, parameters set, amounts, formulas,
additions, texts, parameters removed. So an edit that drops one supplier and
adds another is never ambiguous about which happened first, a formula reads
the parameters set beside it, and a parameter is removed once the formulas set
here have stopped reading it. A parameter set is replaced whole: a unit,
description or uncertainty left out is dropped.

Once every change is in, each line whose formula no longer gives its amount
takes the formula's value, and the answer lists those lines.

A text left unstated stays as it is. A description of no paragraphs clears it.

Added lines are resolved exactly as written ones are – same provider lookup,
same unit rules – because an inventory should not be able to tell how a line
got there.
-}
data ExchangeEditRequest = ExchangeEditRequest
    { eerRemove :: [ExchangeSelectorAPI]
    , eerSetAmounts :: [SetAmountAPI]
    , eerAddInputs :: [TechInputAPI]
    , eerAddBiosphere :: [BioExchangeAPI]
    , eerAddWasteOutputs :: [WasteOutputAPI]
    , eerSetName :: Maybe Text
    , eerSetLocation :: Maybe Text
    , eerSetDescription :: Maybe [Text]
    , eerSetParameters :: [SetParameterAPI]
    , eerSetFormulas :: [SetFormulaAPI]
    , eerRemoveParameters :: [Text]
    , eerNote :: Maybe Text
    -- ^ Why the change was made, kept in the database's history
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ExchangeEditRequest)

{- | A list left unstated is a list of nothing – a document that only removes
a line should not have to say four times that it does nothing else. The
assistant tool already reads its arguments this way.
-}
instance FromJSON ExchangeEditRequest where
    parseJSON = withObject "ExchangeEditRequest" $ \o ->
        ExchangeEditRequest
            <$> o .:? "remove" .!= []
            <*> o .:? "setAmounts" .!= []
            <*> o .:? "addInputs" .!= []
            <*> o .:? "addBiosphere" .!= []
            <*> o .:? "addWasteOutputs" .!= []
            <*> o .:? "setName"
            <*> o .:? "setLocation"
            <*> o .:? "setDescription"
            <*> o .:? "setParameters" .!= []
            <*> o .:? "setFormulas" .!= []
            <*> o .:? "removeParameters" .!= []
            <*> o .:? "note"

{- | What an inventory edit produced: one count per selector, in the order the
selectors were stated. A caller that meant to drop one line and reads three
learns it here, rather than from a score that moved more than it should have.
-}
data ExchangeEditResponse = ExchangeEditResponse
    { eepRemoved :: [Int]
    , eepAmountsSet :: [Int]
    , eepAdded :: Int
    , eepTextsSet :: [Text]
    -- ^ The texts rewritten: @name@, @location@, @description@.
    , eepFormulasSet :: [Int]
    , eepParametersSet :: [Text]
    , eepParametersRemoved :: [Text]
    , eepRestated :: [RestatedLineAPI]
    -- ^ The lines whose amounts followed their formulas once the edits were in.
    , eepTransient :: Bool
    -- ^ True when the edit lives in memory only and an unload would undo it.
    , eepWarnings :: [Text]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ExchangeEditResponse)

-- | A line by its position among the activity's exchanges, and the amount its formula moved it from and to.
data RestatedLineAPI = RestatedLineAPI
    { rsPosition :: Int
    , rsFrom :: Double
    , rsTo :: Double
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped RestatedLineAPI)

-- ---------------------------------------------------------------------------
-- Wire → authoring input
-- ---------------------------------------------------------------------------

{- | Translate the request activities into authoring inputs. Shared by the
HTTP endpoints and the command line, which read the same JSON document.

What can fail here is only shape – a biosphere line that names its flow in
two ways at once or in none, a direction that is neither of the two, a flow
introduced without the unit that is part of its identity; every value is the
validator's to judge. Complaints accumulate across the whole batch and name
the activity they belong to, exactly as the validator's do, so a batch is
fixed in one round trip whichever layer refused it.
-}
toAuthoredActivities :: [ActivityInput] -> Either [Text] [AuthoredActivity]
toAuthoredActivities inputs = case partitionEithers (map toAuthoredActivity inputs) of
    ([], authored) -> Right authored
    (errs, _) -> Left (concat errs)

toAuthoredActivity :: ActivityInput -> Either [Text] AuthoredActivity
toAuthoredActivity ai = case partitionEithers (map toBio (aiBiosphere ai)) of
    (errs@(_ : _), _) -> Left (map here (concat errs))
    ([], bio) ->
        Right
            AuthoredActivity
                { aaName = aiName ai
                , aaLocation = aiLocation ai
                , aaDescription = aiDescription ai
                , aaProductName = aiProductName ai
                , aaProductAmount = aiProductAmount ai
                , aaProductUnit = aiProductUnit ai
                , aaExchanges = map toTechInput (aiInputs ai) <> bio <> map toWasteOutput (aiWasteOutputs ai)
                }
  where
    here msg = aiName ai <> " {" <> aiLocation ai <> "}: " <> msg

toTechInput :: TechInputAPI -> AuthoredExchange
toTechInput ti =
    AuthoredTechInput
        { atiProvider = tiProvider ti
        , atiAmount = tiAmount ti
        , atiUnit = tiUnit ti
        , atiComment = tiComment ti
        }

toWasteOutput :: WasteOutputAPI -> AuthoredExchange
toWasteOutput wo =
    AuthoredWasteOutput
        { awProvider = woProvider wo
        , awAmount = woAmount wo
        , awUnit = woUnit wo
        , awComment = woComment wo
        }

-- | One bad line can be bad both ways at once; both complaints travel.
toBio :: BioExchangeAPI -> Either [Text] AuthoredExchange
toBio be = case (bioFlowRef be, bioDirection (beDirection be)) of
    (Right flow, Right direction) ->
        Right
            AuthoredBio
                { abFlow = flow
                , abDirection = direction
                , abAmount = beAmount be
                , abUnit = beUnit be
                , abComment = beComment be
                }
    (flow, direction) -> Left (leftList flow <> leftList direction)
  where
    leftList = either pure (const [])

{- | Translate an edit request into the changes the domain applies, in the
order the request lists them. Shared by the HTTP endpoint, the command line and
the assistant tool, which all read the same document.

What can fail here is only shape – a selector naming its provider where its
kind names a flow, a kind that is none of the three. Whether a selector reaches
anything is the domain's to judge, and its complaints accumulate the same way,
so a request is fixed in one round trip whichever layer refused it.
-}
toExchangeEdits :: ExchangeEditRequest -> Either [Text] [ExchangeEdit]
toExchangeEdits req = case partitionEithers stated of
    ([], edits) -> Right edits
    (errs, _) -> Left (concat errs)
  where
    stated =
        map (fmap RemoveExchange . toSelector) (eerRemove req)
            <> map toSetParameter (eerSetParameters req)
            <> map toSetAmount (eerSetAmounts req)
            <> map toSetFormula (eerSetFormulas req)
            <> map (Right . AddExchange . toTechInput) (eerAddInputs req)
            <> map (fmap AddExchange . toBio) (eerAddBiosphere req)
            <> map (Right . AddExchange . toWasteOutput) (eerAddWasteOutputs req)
            <> map (Right . SetText) texts
            <> map (Right . RemoveParameter) (eerRemoveParameters req)
    texts =
        concat
            [ ActivityName <$> maybeToList (eerSetName req)
            , ActivityLocation <$> maybeToList (eerSetLocation req)
            , ActivityDescription <$> maybeToList (eerSetDescription req)
            ]
    toSetAmount sa = flip SetAmount (saAmount sa) <$> toSelector (saSelect sa)
    toSetFormula sf = flip SetFormula (sfFormula sf) <$> toSelector (sfSelect sf)

{- | A parameter states a value or a formula, never both: a calculated
parameter's value is what its formula gives, and an uncertainty spreads a
stated value alone.
-}
toSetParameter :: SetParameterAPI -> Either [Text] ExchangeEdit
toSetParameter = fmap (uncurry SetParameter) . toParameter

toParameter :: SetParameterAPI -> Either [Text] (Text, Parameter)
toParameter sp = (spName sp,) . definedBy <$> definition
  where
    definition = case (spValue sp, spFormula sp, spUncertainty sp) of
        (Just value, Nothing, uncertainty) -> Right (InputParameter value uncertainty)
        (Nothing, Just formula, Nothing) -> Right (CalculatedParameter formula Nothing)
        (Nothing, Just _, Just _) -> Left ["the parameter " <> spName sp <> " has a formula, so it takes no uncertainty"]
        (Just _, Just _, _) -> Left ["the parameter " <> spName sp <> " states a value and a formula: it takes one of them"]
        (Nothing, Nothing, _) -> Left ["the parameter " <> spName sp <> " states neither a value nor a formula"]
    definedBy defined =
        Parameter
            { parameterDefinition = defined
            , parameterUnit = spUnit sp
            , parameterDescription = fromMaybe "" (spDescription sp)
            }

{- | The parameters of a database to set, and the names of those to remove
after. A list left unstated is a list of nothing.
-}
data ParametersEditRequest = ParametersEditRequest
    { pqSetParameters :: [SetParameterAPI]
    , pqRemoveParameters :: [Text]
    , pqNote :: Maybe Text
    -- ^ Why the change was made, kept in the database's history
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ParametersEditRequest)

instance FromJSON ParametersEditRequest where
    parseJSON = withObject "ParametersEditRequest" $ \o ->
        ParametersEditRequest <$> o .:? "setParameters" .!= [] <*> o .:? "removeParameters" .!= [] <*> o .:? "note"

-- | What an edit of a database's parameters produced.
data ParametersEditResponse = ParametersEditResponse
    { prsParametersSet :: [Text]
    , prsParametersRemoved :: [Text]
    , prsRestatedProcesses :: [Text]
    -- ^ The processes whose amounts or parameter values followed, by process id.
    , prsTransient :: Bool
    , prsWarnings :: [Text]
    }
    deriving (Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ParametersEditResponse)

-- | The parameters to set, then the names to remove, as the domain takes them.
toParameterEdits :: ParametersEditRequest -> Either [Text] [(Text, Maybe Parameter)]
toParameterEdits req = case partitionEithers (map toParameter (pqSetParameters req)) of
    ([], set) -> Right (map (fmap Just) set <> map (,Nothing) (pqRemoveParameters req))
    (errs, _) -> Left (concat errs)

{- | An input or a waste selector names a provider, or a flow and perhaps a
location; a biosphere selector names a flow. Sending both a provider and a
flow, or a key the kind does not take, is refused rather than resolved by
precedence: a caller who meant a flow and typed a provider would otherwise
edit whatever the provider happened to name.
-}
toSelector :: ExchangeSelectorAPI -> Either [Text] ExchangeSelector
toSelector es = case (T.toLower (T.strip (esKind es)), esProvider es, esFlow es, esLocation es) of
    ("input", Just provider, Nothing, Nothing) -> Right (SelectInput provider)
    ("waste", Just provider, Nothing, Nothing) -> Right (SelectWaste provider)
    ("input", Nothing, Just flowId, location) -> (`SelectUnlinkedInput` fmap readExchangeLocation location) <$> flowNamed flowId
    ("waste", Nothing, Just flowId, location) -> (`SelectUnlinkedWaste` fmap readExchangeLocation location) <$> flowNamed flowId
    ("biosphere", Nothing, Just flowId, Nothing) -> SelectBiosphere <$> flowNamed flowId
    (kind, _, _, _)
        | kind `elem` ["input", "waste"] ->
            Left ["a selector of kind " <> kind <> " names its provider, or its flow and perhaps its location, and nothing else"]
        | kind == "biosphere" ->
            Left ["a biosphere selector names its flow, and nothing else"]
        | otherwise ->
            Left ["unknown selector kind: " <> kind <> " (expected input|waste|biosphere)"]
  where
    flowNamed :: Text -> Either [Text] UUID
    flowNamed flowId = maybe (Left ["not a flow identifier: " <> flowId]) Right (UUID.fromText flowId)

-- | A selector as the wire writes it, which 'toSelector' reads back.
selectorAPI :: ExchangeSelector -> ExchangeSelectorAPI
selectorAPI = \case
    SelectInput provider -> ExchangeSelectorAPI "input" (Just provider) Nothing Nothing
    SelectWaste provider -> ExchangeSelectorAPI "waste" (Just provider) Nothing Nothing
    SelectBiosphere flowId -> ExchangeSelectorAPI "biosphere" Nothing (Just (UUID.toText flowId)) Nothing
    SelectUnlinkedInput flowId location -> ExchangeSelectorAPI "input" Nothing (Just (UUID.toText flowId)) (locationCode <$> location)
    SelectUnlinkedWaste flowId location -> ExchangeSelectorAPI "waste" Nothing (Just (UUID.toText flowId)) (locationCode <$> location)

{- | A biosphere line names a flow by identifier, or in words. Both at once is
ambiguous and neither says nothing, so both are refused instead of picking a
winner.
-}
bioFlowRef :: BioExchangeAPI -> Either Text FlowRef
bioFlowRef be = case (beFlow be, beName be) of
    (Just _, Just _) -> Left "a biosphere exchange names its flow by identifier or in words, not both"
    (Nothing, Nothing) -> Left "a biosphere exchange needs either a flow identifier or a name, a compartment and a unit"
    (Just flowId, Nothing) ->
        maybe (Left (notAnIdentifier flowId)) (Right . FlowById) (UUID.fromText flowId)
    (Nothing, Just name) -> case (beCompartment be, beUnit be) of
        (Nothing, _) -> Left ("biosphere flow \"" <> name <> "\" needs a compartment (air, water, soil, natural resource)")
        -- The unit is half of what makes a flow named in words that flow, both
        -- when the words reach one the database declares and when they mint a
        -- new one (see 'Database.Author.authoredBioFlowUUID'), so it cannot be
        -- defaulted.
        (_, Nothing) -> Left ("biosphere flow \"" <> name <> "\" needs a unit")
        (Just medium, Just unit) -> do
            reading <- first (T.pack . unknownMedium) (parseMedium medium)
            Right $
                FlowByName
                    name
                    Compartment{compartmentName = reading, compartmentSub = beSubCompartment be}
                    unit

{- | A name written into the identifier field is the common mistake, so the
refusal says where identifiers come from and how a name is written instead.
-}
notAnIdentifier :: Text -> Text
notAnIdentifier flowId =
    "not a flow identifier: "
        <> flowId
        <> " (an identifier is the UUID a flow search returns; a flow written in words is stated as its name, compartment and unit instead)"

bioDirection :: Text -> Either Text BioDirection
bioDirection raw = case T.toLower (T.strip raw) of
    "resource" -> Right Resource
    "emission" -> Right Emission
    other -> Left ("unknown biosphere direction: " <> other <> " (expected resource|emission)")
