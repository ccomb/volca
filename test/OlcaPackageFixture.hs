{-# LANGUAGE OverloadedStrings #-}

{- | A small openLCA package, written into a directory by the test that reads
it, with its identifiers. Each process is there for one case the reader must
handle; the numbers expected of them live with the tests.

The JSON is written as openLCA writes it: references as @{"@id": …}@, and a
boolean only when it is true.
-}
module OlcaPackageFixture (
    writePackage,
    kgU,
    lbU,
    tU,
    mjU,
    kwhU,
    massG,
    energyG,
    massP,
    energyP,
    steelF,
    electricityF,
    gasF,
    slagF,
    heatF,
    powerF,
    ashF,
    recycledF,
    widgetF,
    landF,
    tieF,
    thingF,
    sortedF,
    pairF,
    co2F,
    oreF,
    zincF,
    serviceF,
    gasP,
    elecB,
    elecB2,
    steelA,
    slagD,
    cogenF,
    cogenG,
    boilerH,
    recyclingI,
    formulasJ,
    tieK1,
    tieK2,
    tieL,
    sortingM,
    pairN,
) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- | Identifiers that read as what they are when a test prints one.
uid :: Word -> UUID
uid n = UUID.fromWords 0 0 0 (fromIntegral n)

kgU, lbU, tU, mjU, kwhU, massG, energyG, massP, energyP :: UUID
kgU = uid 1
lbU = uid 2
tU = uid 3
mjU = uid 4
kwhU = uid 5
massG = uid 6
energyG = uid 7
massP = uid 8
energyP = uid 9

steelF, electricityF, gasF, slagF, heatF, powerF, ashF, recycledF, widgetF, landF, tieF, thingF, sortedF, pairF :: UUID
steelF = uid 101
electricityF = uid 102
gasF = uid 103
slagF = uid 104
heatF = uid 105
powerF = uid 106
ashF = uid 107
recycledF = uid 108
widgetF = uid 109
landF = uid 110
tieF = uid 111
thingF = uid 112
sortedF = uid 113
pairF = uid 114

co2F, oreF, zincF, serviceF :: UUID
co2F = uid 201
oreF = uid 202
zincF = uid 203
serviceF = uid 204

gasP, elecB, elecB2, steelA, slagD, cogenF, cogenG, boilerH, recyclingI, formulasJ, tieK1, tieK2, tieL, sortingM, pairN :: UUID
gasP = uid 301
elecB = uid 302
elecB2 = uid 303
steelA = uid 304
slagD = uid 305
cogenF = uid 306
cogenG = uid 307
boilerH = uid 308
recyclingI = uid 309
formulasJ = uid 310
tieK1 = uid 311
tieK2 = uid 312
tieL = uid 313
sortingM = uid 314
pairN = uid 315

-- | Write the package into an existing directory.
writePackage :: FilePath -> IO ()
writePackage dir = do
    A.encodeFile (dir </> "openlca.json") (object ["schemaVersion" .= (3 :: Int)])
    write "unit_groups" massG (unitGroup massG [refUnit kgU "kg", unit lbU "lb" 0.45359237, unit tU "t" 1000])
    write "unit_groups" energyG (unitGroup energyG [refUnit mjU "MJ", unit kwhU "kWh" 3.6])
    write "flow_properties" massP (object ["@id" .= massP, "name" .= ("Mass" :: Text), "unitGroup" .= ref massG])
    write "flow_properties" energyP (object ["@id" .= energyP, "name" .= ("Energy" :: Text), "unitGroup" .= ref energyG])
    mapM_ (uncurry (write "flows")) flows
    write "parameters" (uid 401) (parameterDoc "leak_method" (Left 1))
    mapM_ (\p -> write "processes" (pcId p) (processDoc p)) processes
  where
    write :: FilePath -> UUID -> Value -> IO ()
    write folder docId doc = do
        createDirectoryIfMissing True (dir </> folder)
        A.encodeFile (dir </> folder </> (UUID.toString docId <> ".json")) doc

ref :: UUID -> Value
ref i = object ["@id" .= i]

unitGroup :: UUID -> [Value] -> Value
unitGroup gid units = object ["@id" .= gid, "name" .= ("units" :: Text), "units" .= units]

refUnit :: UUID -> Text -> Value
refUnit i name = object ["@id" .= i, "name" .= name, "conversionFactor" .= (1 :: Double), "isRefUnit" .= True]

unit :: UUID -> Text -> Double -> Value
unit i name factor = object ["@id" .= i, "name" .= name, "conversionFactor" .= factor]

flows :: [(UUID, Value)]
flows =
    [ product steelF "steel" massP
    , product electricityF "electricity" energyP
    , (gasF, flowDoc gasF "gas" "PRODUCT_FLOW" "Fuels" [refProperty massP, otherProperty energyP 50])
    , (slagF, flowDoc slagF "slag" "WASTE_FLOW" "Waste" [refProperty massP])
    , product heatF "heat" energyP
    , product powerF "power" energyP
    , product ashF "ash" massP
    , product recycledF "recycled steel" massP
    , product widgetF "widget" massP
    , product landF "land occupation" massP
    , product tieF "tie" massP
    , product thingF "thing" massP
    , product sortedF "sorted ore" massP
    , product pairF "pair" massP
    , elementary co2F "carbon dioxide" "Elementary flows/emission/air"
    , elementary oreF "iron ore" "Elementary flows/resource/ground"
    , elementary zincF "zinc" "Elementary flows/emission/ground"
    , elementary serviceF "pollination" "Ecosystem Services"
    ]
  where
    product :: UUID -> Text -> UUID -> (UUID, Value)
    product fid name property = (fid, flowDoc fid name "PRODUCT_FLOW" "Products" [refProperty property])

    elementary :: UUID -> Text -> Text -> (UUID, Value)
    elementary fid name category = (fid, flowDoc fid name "ELEMENTARY_FLOW" category [refProperty massP])

flowDoc :: UUID -> Text -> Text -> Text -> [Value] -> Value
flowDoc fid name flowType category properties =
    object ["@id" .= fid, "name" .= name, "flowType" .= flowType, "category" .= category, "flowProperties" .= properties]

refProperty :: UUID -> Value
refProperty property = object ["flowProperty" .= ref property, "conversionFactor" .= (1 :: Double), "isRefFlowProperty" .= True]

otherProperty :: UUID -> Double -> Value
otherProperty property factor = object ["flowProperty" .= ref property, "conversionFactor" .= factor]

-- | An input parameter (a value) or a calculated one (a formula, with the value openLCA stored).
parameterDoc :: Text -> Either Double (Text, Double) -> Value
parameterDoc name value =
    object $
        ["name" .= name, "parameterScope" .= ("GLOBAL_SCOPE" :: Text)] <> case value of
            Left given -> ["isInputParameter" .= True, "value" .= given]
            Right (formula, stored) -> ["isInputParameter" .= False, "formula" .= formula, "value" .= stored]

-- | One exchange line, as a record so the fixture reads as a table.
data Line = Line
    { lnId :: Int
    , lnFlow :: UUID
    , lnAmount :: Double
    , lnFormula :: Maybe Text
    , lnUnit :: UUID
    , lnProperty :: UUID
    , lnInput :: Bool
    , lnAvoided :: Bool
    , lnReference :: Bool
    , lnProvider :: Maybe UUID
    }

-- | The internal ids a cogeneration process gives its heat, power, gas and CO2 lines.
data Numbering = Numbering
    { nuHeat :: Int
    , nuPower :: Int
    , nuGas :: Int
    , nuCo2 :: Int
    }

output, input :: Int -> UUID -> Double -> UUID -> UUID -> Line
output i flow amount u property = Line i flow amount Nothing u property False False False Nothing
input i flow amount u property = (output i flow amount u property){lnInput = True}

reference :: Line -> Line
reference line = line{lnReference = True}

from :: UUID -> Line -> Line
from provider line = line{lnProvider = Just provider}

lineDoc :: Line -> Value
lineDoc l =
    object $
        [ "internalId" .= lnId l
        , "flow" .= ref (lnFlow l)
        , "amount" .= lnAmount l
        , "unit" .= ref (lnUnit l)
        , "flowProperty" .= ref (lnProperty l)
        ]
            <> catMaybes
                [ ("amountFormula" .=) <$> lnFormula l
                , ("defaultProvider" .=) . ref <$> lnProvider l
                , whenTrue "isInput" (lnInput l)
                , whenTrue "isAvoidedProduct" (lnAvoided l)
                , whenTrue "isQuantitativeReference" (lnReference l)
                ]
  where
    whenTrue :: Text -> Bool -> Maybe (Key.Key, Value)
    whenTrue key holds = if holds then Just (Key.fromText key .= True) else Nothing

data Proc = Proc
    { pcId :: UUID
    , pcName :: Text
    , pcType :: Text
    , pcAllocation :: Maybe Text
    , pcLines :: [Line]
    , pcFactors :: [Value]
    , pcParameters :: [Value]
    }

unitProcess :: UUID -> Text -> [Line] -> Proc
unitProcess i name lines' = Proc i name "UNIT_PROCESS" Nothing lines' [] []

processDoc :: Proc -> Value
processDoc p =
    object $
        [ "@id" .= pcId p
        , "name" .= pcName p
        , "category" .= ("Fixture/processes" :: Text)
        , "processType" .= pcType p
        , "exchanges" .= map lineDoc (pcLines p)
        , "allocationFactors" .= pcFactors p
        , "parameters" .= pcParameters p
        ]
            <> maybe [] (\m -> ["defaultAllocationMethod" .= m]) (pcAllocation p)

-- | A factor for one product, or (causal) for one product and one exchange line.
factor :: Text -> UUID -> Maybe Int -> Double -> Value
factor method product line value =
    object $
        ["allocationType" .= method, "product" .= ref product, "value" .= value]
            <> maybe [] (\i -> ["exchange" .= object ["internalId" .= i]]) line

processes :: [Proc]
processes =
    [ unitProcess gasP "gas production" [reference (output 1 gasF 1 kgU massP), output 2 co2F 0.1 kgU massP, input 3 oreF 1 kgU massP]
    , unitProcess
        elecB
        "electricity, unit process"
        [reference (output 1 electricityF 1 mjU energyP), from gasP (input 2 gasF 0.5 mjU energyP), output 3 co2F 0.2 kgU massP]
    , (unitProcess elecB2 "electricity, aggregated" [reference (output 1 electricityF 1 kwhU energyP), output 2 co2F 0.9 kgU massP])
        { pcType = "LCI_RESULT"
        }
    , unitProcess
        steelA
        "steel production"
        [ reference (output 1 steelF 1 tU massP)
        , from elecB (input 2 electricityF 500 kwhU energyP)
        , output 3 co2F 220.46226218487757 lbU massP
        , output 4 slagF 50 kgU massP
        ]
    , unitProcess
        slagD
        "slag treatment"
        [reference (input 1 slagF 1 kgU massP), output 2 zincF 0.001 kgU massP, input 3 electricityF 0.1 mjU energyP]
    , (cogeneration cogenF "cogeneration, physical" Numbering{nuHeat = 1, nuPower = 2, nuGas = 3, nuCo2 = 4})
        { pcAllocation = Just "PHYSICAL_ALLOCATION"
        , pcFactors =
            [ factor "PHYSICAL_ALLOCATION" heatF Nothing 0.6
            , factor "PHYSICAL_ALLOCATION" powerF Nothing 0.4
            , factor "ECONOMIC_ALLOCATION" heatF Nothing 0.3
            , factor "ECONOMIC_ALLOCATION" powerF Nothing 0.7
            ]
        }
    , (cogeneration cogenG "cogeneration, causal" Numbering{nuHeat = 10, nuPower = 3, nuGas = 7, nuCo2 = 1})
        { pcAllocation = Just "CAUSAL_ALLOCATION"
        , pcFactors =
            [ factor "CAUSAL_ALLOCATION" heatF (Just 1) 0.9
            , factor "CAUSAL_ALLOCATION" powerF (Just 1) 0.1
            , factor "CAUSAL_ALLOCATION" heatF (Just 7) 0.5
            , -- A factor for a line that names no product, as some packages write them.
              object ["allocationType" .= ("CAUSAL_ALLOCATION" :: Text), "exchange" .= object ["internalId" .= (1 :: Int)], "value" .= (0 :: Double)]
            ]
        }
    , unitProcess
        boilerH
        "boiler"
        [reference (output 1 heatF 10 mjU energyP), output 2 ashF 0.2 kgU massP, from gasP (input 3 gasF 0.2 kgU massP), output 4 co2F 0.5 kgU massP]
    , unitProcess
        recyclingI
        "steel recycling"
        [ reference (output 1 recycledF 1 kgU massP)
        , from elecB (input 2 electricityF 1 mjU energyP)
        , (from steelA (input 3 steelF 0.8 kgU massP)){lnAvoided = True}
        , output 4 co2F 0.05 kgU massP
        ]
    , (unitProcess formulasJ "widget assembly" formulaLines)
        { pcParameters =
            [ parameterDoc "a" (Left 2)
            , parameterDoc "b" (Right ("if(leak_method == 1; a * 3; missing_name)", 6))
            , parameterDoc "c" (Right ("-2^2 + log(100)", 6))
            , -- Stored 2 where the formula gives 1: openLCA recomputes, and the oracle shows it.
              parameterDoc "d" (Right ("2^3^2 / 64", 2))
            ]
        }
    , unitProcess tieK1 "tie, first" [reference (output 1 tieF 1 kgU massP), output 2 co2F 1 kgU massP]
    , unitProcess tieK2 "tie, second" [reference (output 1 tieF 1 kgU massP), output 2 co2F 2 kgU massP]
    , unitProcess tieL "thing making" [reference (output 1 thingF 1 kgU massP), input 2 tieF 1 kgU massP]
    , -- Elementary flows on the side opposite to their kind: ore returned, CO2 captured, a service with no compartment taken.
      unitProcess
        sortingM
        "ore sorting"
        [ reference (output 1 sortedF 1 kgU massP)
        , input 2 oreF 1 kgU massP
        , output 3 oreF 0.25 kgU massP
        , input 4 co2F 0.4 kgU massP
        , output 5 co2F 1 kgU massP
        , input 6 serviceF 0.3 kgU massP
        ]
    , -- Its product on two lines.
      unitProcess pairN "pair making" [reference (output 1 pairF 1 kgU massP), output 2 pairF 1 kgU massP, input 3 oreF 1 kgU massP]
    ]
  where
    -- The same four lines in both cogeneration processes, numbered as given.
    cogeneration :: UUID -> Text -> Numbering -> Proc
    cogeneration i name numbering =
        unitProcess
            i
            name
            [ reference (output (nuHeat numbering) heatF 2 mjU energyP)
            , output (nuPower numbering) powerF 1 mjU energyP
            , from gasP (input (nuGas numbering) gasF 0.1 kgU massP)
            , output (nuCo2 numbering) co2F 1 kgU massP
            ]

    formulaLines :: [Line]
    formulaLines =
        [ reference (output 1 widgetF 1 kgU massP)
        , (output 2 co2F 0.6 kgU massP){lnFormula = Just "b / 10"}
        , (input 3 oreF 0.05 kgU massP){lnFormula = Just "c * d / 100"}
        , input 4 landF 3 kgU massP
        , (input 5 electricityF 1 mjU energyP){lnFormula = Just "max(1; 2; 0.5) - 1"}
        , (output 6 zincF 0.001 kgU massP){lnFormula = Just "random()"}
        ]
