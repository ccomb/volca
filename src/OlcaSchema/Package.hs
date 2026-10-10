{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | An openLCA JSON-LD package (schema version 2 or 3) as its files are written:
one aeson instance per kind of document, read from the folders of an
extracted zip. Nothing here computes; "OlcaSchema.Parser" turns a 'Package'
into a database.

A malformed document refuses the whole package, naming its file and what
failed: a database read with one process quietly missing computes wrong
numbers for every process that buys from it.

openLCA leaves a field out when it holds its default (a boolean at false, an
amount at zero), so those read with that default rather than as an error.
-}
module OlcaSchema.Package (
    Package (..),
    UnitGroup (..),
    UnitEntry (..),
    Flow (..),
    FlowType (..),
    Process (..),
    ProcessType (..),
    AllocationMethod (..),
    RawExchange (..),
    Side (..),
    AllocationFactor (..),
    Parameter (..),
    ParameterValue (..),
    Unread (..),
    isOlcaPackage,
    readPackage,
) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Aeson (FromJSON (..), withObject, withText, (.!=), (.:), (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A (Parser)
import Data.Bifunctor (first)
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)

import Data.Indexing (uniqueIndex)
import System.FilePath (takeExtension, (</>))

-- | Every document the reader needs, by kind.
data Package = Package
    { pkUnitGroups :: !(M.Map UUID UnitGroup)
    , pkPropertyGroups :: !(M.Map UUID UUID)
    -- ^ Each flow property's unit group
    , pkFlows :: !(M.Map UUID Flow)
    , pkLocations :: !(M.Map UUID Text)
    -- ^ Each location's code
    , pkGlobals :: ![Parameter]
    , pkProcesses :: ![Process]
    }

data UnitGroup = UnitGroup
    { ugId :: !UUID
    , ugReference :: !UUID
    , ugUnits :: !(M.Map UUID UnitEntry)
    }
    deriving (Generic, NFData)

-- | A unit, and how many of the group's reference unit one of it is.
data UnitEntry = UnitEntry
    { ueName :: !Text
    , ueFactor :: !Double
    }
    deriving (Eq, Show, Generic, NFData)

data FlowType = ElementaryFlow | ProductFlow | WasteFlow
    deriving (Eq, Show, Generic, NFData)

data Flow = Flow
    { flId :: !UUID
    , flName :: !Text
    , flType :: !FlowType
    , flCategory :: !Text
    , flCas :: !(Maybe Text)
    , flReference :: !UUID
    -- ^ Its reference flow property, which its amounts are stated in once read
    , flFactors :: !(M.Map UUID Double)
    -- ^ Per flow property, how much of it one reference unit of the flow holds
    }
    deriving (Generic, NFData)

data ProcessType = UnitProcess | LciResult
    deriving (Eq, Show, Generic, NFData)

data AllocationMethod = Physical | Economic | Causal | NoAllocation
    deriving (Eq, Show, Generic, NFData)

-- | What openLCA records but this reader does not read yet, counted by the load.
data Unread = Uncertainty | DataQuality | SocialAspects | Costs
    deriving (Eq, Ord, Show, Generic, NFData)

data Process = Process
    { prId :: !UUID
    , prName :: !Text
    , prCategory :: !Text
    , prType :: !ProcessType
    , prLocation :: !(Maybe UUID)
    , prDescription :: !(Maybe Text)
    , prAllocation :: !(Maybe AllocationMethod)
    , prExchanges :: ![RawExchange]
    , prFactors :: ![AllocationFactor]
    , prParameters :: ![Parameter]
    , prUnread :: ![Unread]
    -- ^ One entry per thing not read, the process's and its exchanges'
    }
    deriving (Generic, NFData)

{- | Which way a line goes. openLCA stores an avoided product as an input
marked avoided, and computes it as an output; it is a side of its own here so
no reader can take it for an ordinary input.
-}
data Side = Produced | Consumed | Avoided
    deriving (Eq, Show, Generic, NFData)

data RawExchange = RawExchange
    { rxInternalId :: !Int
    , rxFlow :: !UUID
    , rxAmount :: !Double
    , rxFormula :: !(Maybe Text)
    , rxUnit :: !UUID
    , rxProperty :: !(Maybe UUID)
    , rxSide :: !Side
    , rxReference :: !Bool
    , rxProvider :: !(Maybe UUID)
    , rxLocation :: !(Maybe UUID)
    , rxDescription :: !(Maybe Text)
    , rxUnread :: ![Unread]
    }
    deriving (Generic, NFData)

data AllocationFactor = AllocationFactor
    { afMethod :: !AllocationMethod
    , afProduct :: !(Maybe UUID)
    -- ^ Nothing where the file names none: such a factor applies to no product
    , afExchange :: !(Maybe Int)
    -- ^ The internal id of the line a causal factor is for
    , afValue :: !Double
    , afFormula :: !(Maybe Text)
    }
    deriving (Generic, NFData)

data ParameterValue
    = InputValue !Double
    | -- | A formula, and the value openLCA stored beside it.
      Calculated !Text !Double
    deriving (Generic, NFData)

data Parameter = Parameter
    { paName :: !Text
    , paValue :: !ParameterValue
    }
    deriving (Generic, NFData)

reference :: A.Value -> A.Parser UUID
reference = withObject "reference" (.: "@id")

optionalReference :: A.Object -> A.Key -> A.Parser (Maybe UUID)
optionalReference o key = o .:? key >>= traverse reference

-- | A text field where openLCA writes an empty string for none.
nonBlank :: Maybe Text -> Maybe Text
nonBlank = (>>= \t -> if T.null (T.strip t) then Nothing else Just t)

instance FromJSON UnitEntry where
    parseJSON = withObject "Unit" $ \o -> UnitEntry <$> o .: "name" <*> o .: "conversionFactor"

-- | A unit as its group lists it, before the group picks its reference.
data UnitDoc = UnitDoc
    { udId :: UUID
    , udEntry :: UnitEntry
    , udReference :: Bool
    }
    deriving (Generic, NFData)

instance FromJSON UnitDoc where
    parseJSON v = withObject "Unit" (\o -> UnitDoc <$> o .: "@id" <*> parseJSON v <*> o .:? "isRefUnit" .!= False) v

instance FromJSON UnitGroup where
    parseJSON = withObject "UnitGroup" $ \o -> do
        groupId <- o .: "@id"
        units <- o .: "units"
        case [udId u | u <- units, udReference u] of
            [ref] -> pure UnitGroup{ugId = groupId, ugReference = ref, ugUnits = unitTable units}
            refs -> fail (show (length refs) <> " reference units where one is needed")

data PropertyDoc = PropertyDoc UUID UUID
    deriving (Generic, NFData)

instance FromJSON PropertyDoc where
    parseJSON = withObject "FlowProperty" $ \o -> PropertyDoc <$> o .: "@id" <*> (o .: "unitGroup" >>= reference)

data LocationDoc = LocationDoc UUID Text
    deriving (Generic, NFData)

instance FromJSON LocationDoc where
    parseJSON = withObject "Location" $ \o -> LocationDoc <$> o .: "@id" <*> o .:? "code" .!= ""

instance FromJSON FlowType where
    parseJSON = withText "FlowType" $ \case
        "ELEMENTARY_FLOW" -> pure ElementaryFlow
        "PRODUCT_FLOW" -> pure ProductFlow
        "WASTE_FLOW" -> pure WasteFlow
        other -> fail ("unknown flow type " <> T.unpack other)

data FactorDoc = FactorDoc
    { fdProperty :: UUID
    , fdFactor :: Double
    , fdReference :: Bool
    }
    deriving (Generic, NFData)

instance FromJSON FactorDoc where
    parseJSON = withObject "FlowPropertyFactor" $ \o ->
        FactorDoc
            <$> (o .: "flowProperty" >>= reference)
            -- openLCA's own default for a factor it leaves out
            <*> o .:? "conversionFactor" .!= 1
            <*> o .:? "isRefFlowProperty" .!= False

instance FromJSON Flow where
    parseJSON = withObject "Flow" $ \o -> do
        factors <- o .: "flowProperties"
        ref <- case [fdProperty f | f <- factors, fdReference f] of
            [one] -> pure one
            refs -> fail (show (length refs) <> " reference flow properties where one is needed")
        fid <- o .: "@id"
        name <- o .: "name"
        flowType <- o .: "flowType"
        category <- o .:? "category" .!= ""
        cas <- o .:? "cas"
        pure
            Flow
                { flId = fid
                , flName = name
                , flType = flowType
                , flCategory = category
                , flCas = nonBlank cas
                , flReference = ref
                , flFactors = factorTable factors
                }

unitTable :: [UnitDoc] -> M.Map UUID UnitEntry
unitTable units = M.fromList [(udId u, udEntry u) | u <- units]

factorTable :: [FactorDoc] -> M.Map UUID Double
factorTable factors = M.fromList [(fdProperty f, fdFactor f) | f <- factors]

instance FromJSON ProcessType where
    parseJSON = withText "ProcessType" $ \case
        "UNIT_PROCESS" -> pure UnitProcess
        "LCI_RESULT" -> pure LciResult
        other -> fail ("unknown process type " <> T.unpack other)

instance FromJSON AllocationMethod where
    parseJSON = withText "AllocationMethod" $ \case
        "PHYSICAL_ALLOCATION" -> pure Physical
        "ECONOMIC_ALLOCATION" -> pure Economic
        "CAUSAL_ALLOCATION" -> pure Causal
        "NO_ALLOCATION" -> pure NoAllocation
        other -> fail ("unknown allocation method " <> T.unpack other)

instance FromJSON Parameter where
    parseJSON = withObject "Parameter" $ \o -> do
        name <- o .: "name"
        -- openLCA reads a parameter with no flag as an input parameter.
        given <- o .:? "isInputParameter" .!= True
        value <- o .:? "value" .!= 0
        formula <- o .:? "formula"
        pure
            Parameter
                { paName = name
                , paValue = case (given, nonBlank formula) of
                    (False, Just f) -> Calculated f value
                    (False, Nothing) -> InputValue value
                    (True, _) -> InputValue value
                }

instance FromJSON RawExchange where
    parseJSON = withObject "Exchange" $ \o -> do
        isInput <- o .:? "isInput" .!= False
        avoided <- o .:? "isAvoidedProduct" .!= False
        uncertainty <- o .:? "uncertainty" :: A.Parser (Maybe A.Value)
        quality <- o .:? "dqEntry" :: A.Parser (Maybe Text)
        cost <- o .:? "costValue" :: A.Parser (Maybe Double)
        internalId <- o .: "internalId"
        flow <- o .: "flow" >>= reference
        amount <- o .:? "amount" .!= 0
        formula <- o .:? "amountFormula"
        unit <- o .: "unit" >>= reference
        property <- optionalReference o "flowProperty"
        isReference <- o .:? "isQuantitativeReference" .!= False
        provider <- optionalReference o "defaultProvider"
        location <- optionalReference o "location"
        description <- o .:? "description"
        pure
            RawExchange
                { rxInternalId = internalId
                , rxFlow = flow
                , rxAmount = amount
                , rxFormula = nonBlank formula
                , rxUnit = unit
                , rxProperty = property
                , rxSide = if avoided then Avoided else if isInput then Consumed else Produced
                , rxReference = isReference
                , rxProvider = provider
                , rxLocation = location
                , rxDescription = nonBlank description
                , rxUnread = [Uncertainty | isJust uncertainty] <> [DataQuality | isJust (nonBlank quality)] <> [Costs | isJust cost]
                }

instance FromJSON AllocationFactor where
    parseJSON = withObject "AllocationFactor" $ \o -> do
        method <- o .: "allocationType"
        sold <- optionalReference o "product"
        line <- o .:? "exchange" >>= traverse (withObject "exchange" (.: "internalId"))
        value <- o .:? "value" .!= 0
        formula <- o .:? "formula"
        pure AllocationFactor{afMethod = method, afProduct = sold, afExchange = line, afValue = value, afFormula = nonBlank formula}

instance FromJSON Process where
    parseJSON = withObject "Process" $ \o -> do
        pid <- o .: "@id"
        name <- o .: "name"
        category <- o .:? "category" .!= ""
        processType <- o .:? "processType" .!= UnitProcess
        location <- optionalReference o "location"
        description <- o .:? "description"
        allocation <- o .:? "defaultAllocationMethod"
        exchanges <- o .:? "exchanges" .!= []
        factors <- o .:? "allocationFactors" .!= []
        parameters <- o .:? "parameters" .!= []
        quality <- o .:? "dqEntry" :: A.Parser (Maybe Text)
        social <- o .:? "socialAspects" .!= ([] :: [A.Value])
        pure
            Process
                { prId = pid
                , prName = name
                , prCategory = category
                , prType = processType
                , prLocation = location
                , prDescription = nonBlank description
                , prAllocation = allocation
                , prExchanges = exchanges
                , prFactors = factors
                , prParameters = parameters
                , prUnread = [DataQuality | isJust (nonBlank quality)] <> [SocialAspects | not (null social)] <> concatMap rxUnread exchanges
                }

newtype SchemaVersion = SchemaVersion Int
    deriving (Generic, NFData)

instance FromJSON SchemaVersion where
    parseJSON = withObject "openlca.json" $ \o -> SchemaVersion <$> o .: "schemaVersion"

-- | Whether a directory is an openLCA package: its root holds @openlca.json@.
isOlcaPackage :: FilePath -> IO Bool
isOlcaPackage dir = doesFileExist (dir </> "openlca.json")

-- | Read the documents of an extracted package. A missing folder is a package without that kind of document.
readPackage :: FilePath -> IO (Either Text Package)
readPackage dir = runExceptT $ do
    SchemaVersion version <- ExceptT (decodeFile (dir </> "openlca.json"))
    -- Version 3 changed only how a process records its reviews, which this reader does not read.
    unless (version `elem` [2, 3]) $
        throwE ("openlca.json: schema version " <> T.pack (show version) <> ", where this reader knows versions 2 and 3")
    groups <- ExceptT (decodeFolder (dir </> "unit_groups"))
    properties <- ExceptT (decodeFolder (dir </> "flow_properties"))
    flows <- ExceptT (decodeFolder (dir </> "flows"))
    locations <- ExceptT (decodeFolder (dir </> "locations"))
    globals <- ExceptT (decodeFolder (dir </> "parameters"))
    processes <- ExceptT (decodeFolder (dir </> "processes"))
    unitGroups <- unique "unit groups" [(ugId g, g) | g <- groups]
    propertyGroups <- unique "flow properties" [(p, g) | PropertyDoc p g <- properties]
    flowIndex <- unique "flows" [(flId f, f) | f <- flows]
    locationIndex <- unique "locations" [(l, code) | LocationDoc l code <- locations]
    _ <- unique "processes" [(prId p, ()) | p <- processes]
    pure
        Package
            { pkUnitGroups = unitGroups
            , pkPropertyGroups = propertyGroups
            , pkFlows = flowIndex
            , pkLocations = locationIndex
            , pkGlobals = globals
            , pkProcesses = processes
            }
  where
    -- Two documents with one identifier: which one openLCA would keep is not ours to guess.
    unique :: Text -> [(UUID, v)] -> ExceptT Text IO (M.Map UUID v)
    unique kind rows = case uniqueIndex rows of
        Right index -> pure index
        Left repeated -> throwE (kind <> " repeat an identifier: " <> T.intercalate ", " (map UUID.toText (NE.toList repeated)))

{- | A document, evaluated through: what aeson hands back is a promise holding
the parsed text (a line's identifiers, amounts and flags), several times the
size of the values it stands for, and a package keeps every line until the
database is built.
-}
decodeFile :: (FromJSON a, NFData a) => FilePath -> IO (Either Text a)
decodeFile path = evaluate . force . first (\why -> T.pack (path <> ": " <> why)) =<< A.eitherDecodeFileStrict' path

-- | Every @.json@ document of a folder, in name order; a package ships other files beside them.
decodeFolder :: (FromJSON a, NFData a) => FilePath -> IO (Either Text [a])
decodeFolder folder = do
    present <- doesDirectoryExist folder
    if present
        then do
            names <- sort . filter ((== ".json") . takeExtension) <$> listDirectory folder
            sequence <$> traverse (decodeFile . (folder </>)) names
        else pure (Right [])
