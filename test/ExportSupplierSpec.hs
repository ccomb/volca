{-# LANGUAGE OverloadedStrings #-}

{- | The formats that name a supplier rather than point at it: each input must
come back from the supplier it was bought from, within the file and across a
dependency, or the export must refuse.
-}
module ExportSupplierSpec (spec) where

import qualified BrightwayExcel.Writer as BE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Data.Word (Word32)
import Database (buildDatabaseWithMatrices)
import Database.CrossLinking (buildIndexedDatabase, locationHierarchy)
import Database.ExportSupplier (Dependencies (..), Elsewhere (..), dependenciesOf, noDependencies)
import Database.Loader (defaultLoadOptions, fixActivityLinksWithCrossDB, loadDatabase, loadSimaProCSV)
import ILCD.Parser (parseILCDDirectory)
import qualified ILCD.Writer as ILCD
import qualified SimaPro.Writer as SP
import SynonymDB (emptySynonymDB)
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Test.Hspec
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    describe "SimaPro CSV" $ do
        it "brings each gin back to its own farm" $ do
            db <- loadSimaPro =<< simaPro noDependencies (fixture "IN")
            map (supplierOf db) ["gin A", "gin B"] `shouldBe` [Just "farm A", Just "farm B"]

        it "names a supplier of a dependency by its activity and location" $ do
            out <- simaPro dependencies (fixture "IN")
            rowsUnder out "Materials/fuels"
                `shouldSatisfy` any ("power {TR}| power market |;" `T.isPrefixOf`)

        it "refuses two makers of one product with one name at one place" $
            SP.serializeSimaProCSV SP.defaultWriterConfig (renamed "farm B" "farm A" (fixture "IN")) `shouldSatisfy` isLeft

        it "refuses a maker of a shared product stating no location" $
            SP.serializeSimaProCSV SP.defaultWriterConfig (fixture "") `shouldSatisfy` isLeft

        it "refuses a shared product whose name already states a location" $
            SP.serializeSimaProCSV SP.defaultWriterConfig (seedNamed "seed {IN}" (fixture "IN")) `shouldSatisfy` isLeft

        it "refuses a supplier of a dependency stating no location" $
            SP.serializeSimaProCSV
                SP.defaultWriterConfig{SP.wcDependencies = Dependencies (M.singleton (millU, yarnU, 2) (Elsewhere "background" "power market" "" PositiveReference))}
                (fixture "IN")
                `shouldSatisfy` isLeft

        -- No row asks which farm, so nothing has to say it.
        it "leaves a product several activities make by its name when nothing buys it" $ do
            out <- simaPro noDependencies (farmsOnly (fixture ""))
            rowsUnder out "Products" `shouldSatisfy` all ("seed;" `T.isPrefixOf`)

        it "brings a substitution back to the farm it stands in for" $ do
            db <- loadSimaPro =<< simaPro noDependencies (avoiding (fixture "IN"))
            linkedFrom AvoidedProduct db "gin B" `shouldBe` Just "farm A"

    describe "Brightway workbook" $ do
        it "brings each gin back to its own farm" $ do
            db <- loadBrightway (fixture "IN")
            map (supplierOf db) ["gin A", "gin B"] `shouldBe` [Just "farm A", Just "farm B"]

        it "names a supplier of a dependency in its name, location and database columns" $
            BE.sheetRows BE.defaultWriterConfig{BE.wcDependencies = dependencies} (fixture "IN")
                `shouldSatisfy` any (\row -> all (`elem` row) [BE.CText "power market", BE.CText "power", BE.CText "TR", BE.CText "background"])

        it "refuses two makers of one product with one name at one place" $
            BE.renderWorkbook BE.defaultWriterConfig (renamed "farm B" "farm A" (fixture "IN")) `shouldSatisfy` isLeft

        it "brings a substitution back to the farm it stands in for" $ do
            db <- loadBrightway (avoiding (fixture "IN"))
            linkedFrom AvoidedProduct db "gin B" `shouldBe` Just "farm A"

    describe "ILCD" $ do
        it "brings each gin back to its own farm when the farms are at two places" $ do
            db <- ilcdRoundTrip (fixture "BR")
            map (supplierOf db) ["gin A", "gin B"] `shouldBe` [Just "farm A", Just "farm B"]

        it "refuses two makers of one product at one place" $
            ILCD.writeILCDArchive ILCD.defaultWriteOptions (fixture "IN") `shouldSatisfy` isLeft

        it "refuses a supplier in a dependency, which it cannot name" $
            ILCD.writeILCDArchive ILCD.defaultWriteOptions{ILCD.woDependencies = dependencies} (fixture "BR") `shouldSatisfy` isLeft

    describe "dependenciesOf" $
        it "finds the input the engine linked, at its place among the exported exchanges" $ do
            let background = buildIndexedDatabase "background" emptySynonymDB backgroundDb
            (linked, stats) <- fixActivityLinksWithCrossDB [background] emptySynonymDB defaultUnitConfig locationHierarchy GeoGlobal (fixture "BR")
            built <- either (fail . T.unpack) pure =<< buildDatabaseWithMatrices (BuildInputs defaultUnitConfig M.empty Declared []) linked
            let db = built{dbCrossDBLinks = cdlLinks stats}
                exported = toSimpleDatabase db
            Dependencies found <- either (fail . T.unpack) pure (dependenciesOf (M.singleton "background" background) db)
            [ (elsewhere, flowName exported ex)
              | ((act, prod, i), elsewhere) <- M.toList found
              , Just consumer <- [M.lookup (act, prod) (sdbActivities exported)]
              , ex <- take 1 (drop i (exchanges consumer))
              ]
                `shouldBe` [(Elsewhere "background" "power market" "TR" PositiveReference, "power")]
            out <- simaPro (Dependencies found) exported
            rowsUnder out "Materials/fuels"
                `shouldSatisfy` any ("power {TR}| power market |;" `T.isPrefixOf`)

-- ---------------------------------------------------------------------------
-- Writing and reading back
-- ---------------------------------------------------------------------------

simaPro :: Dependencies -> SimpleDatabase -> IO BS.ByteString
simaPro deps = either (fail . T.unpack) pure . SP.serializeSimaProCSV SP.defaultWriterConfig{SP.wcDependencies = deps}

loadSimaPro :: BS.ByteString -> IO SimpleDatabase
loadSimaPro bytes = withSystemTempFile "export-supplier.csv" $ \path h -> do
    BS.hPut h bytes
    hClose h
    either (fail . T.unpack) pure =<< loadSimaProCSV (defaultLoadOptions defaultUnitConfig) path

loadBrightway :: SimpleDatabase -> IO SimpleDatabase
loadBrightway db = withSystemTempFile "export-supplier.xlsx" $ \path h -> do
    bytes <- either (fail . T.unpack) pure (BE.renderWorkbook BE.defaultWriterConfig db)
    BL.hPut h bytes
    hClose h
    either (fail . T.unpack) pure =<< loadDatabase defaultUnitConfig path

ilcdRoundTrip :: SimpleDatabase -> IO SimpleDatabase
ilcdRoundTrip db = withSystemTempDirectory "export-supplier-ilcd" $ \dir -> do
    _ <- either (fail . T.unpack) pure =<< ILCD.writeILCDDatabase ILCD.defaultWriteOptions dir db
    either (fail . T.unpack) pure =<< parseILCDDirectory defaultUnitConfig Declared dir

-- | The name of the activity a gin buys its seed from, once read back.
supplierOf :: SimpleDatabase -> Text -> Maybe Text
supplierOf = linkedFrom Input

-- | The name of the activity the one seed exchange of this role of a gin links to.
linkedFrom :: TechRole -> SimpleDatabase -> Text -> Maybe Text
linkedFrom role db gin =
    case [ activityName supplier
         | act <- M.elems (sdbActivities db)
         , activityName act == gin
         , TechnosphereExchange{techRole = role', techActivityLinkId = Just link, techFlowId = flow} <- exchanges act
         , role' == role
         , Just "seed" == fmap (T.takeWhile (/= ' ') . tfName) (M.lookup flow (sdbTechFlows db))
         , ((supplierUUID, _), supplier) <- M.toList (sdbActivities db)
         , supplierUUID == link
         ] of
        [name] -> Just name
        _ -> Nothing

flowName :: SimpleDatabase -> Exchange -> Text
flowName db ex = maybe "?" tfName (M.lookup (exchangeFlowId ex) (sdbTechFlows db))

-- | The rows under a section header in every process of a SimaPro export, carriage returns off.
rowsUnder :: BS.ByteString -> Text -> [Text]
rowsUnder out header = go (map (T.dropWhileEnd (== '\r')) (T.lines (TE.decodeUtf8 out)))
  where
    go ls = case drop 1 (dropWhile (/= header) ls) of
        [] -> []
        rest -> let (rows, after) = break T.null rest in rows ++ go after

-- ---------------------------------------------------------------------------
-- Fixture
-- ---------------------------------------------------------------------------

-- | The mill's power, bought from an activity of a database it depends on.
dependencies :: Dependencies
dependencies = Dependencies (M.singleton (millU, yarnU, 2) (Elsewhere "background" "power market" "TR" PositiveReference))

{- | Two farms making one product, « seed », farm A in IN and farm B at the
place given; each gin buys its seed from one of them, and the mill buys
power nobody in the file makes.
-}
fixture :: Text -> SimpleDatabase
fixture farmBAt =
    simple
        [ ((farmAU, seedU), activity "farm A" "IN" [reference seedU])
        , ((farmBU, seedU), activity "farm B" farmBAt [reference seedU])
        , ((ginAU, fibreAU), activity "gin A" "IN" [reference fibreAU, input seedU (Just farmAU)])
        , ((ginBU, fibreBU), activity "gin B" "IN" [reference fibreBU, input seedU (Just farmBU)])
        , ((millU, yarnU), activity "mill" "TR" [reference yarnU, input fibreAU (Just ginAU), input powerU Nothing])
        ]
        [(seedU, "seed"), (fibreAU, "fibre A"), (fibreBU, "fibre B"), (yarnU, "yarn"), (powerU, "power")]

-- | The database the mill's power comes from.
backgroundDb :: SimpleDatabase
backgroundDb = simple [((marketU, powerU), activity "power market" "TR" [reference powerU])] [(powerU, "power")]

-- | The two farms alone, whose seed nobody in the file buys.
farmsOnly :: SimpleDatabase -> SimpleDatabase
farmsOnly db = db{sdbActivities = M.filterWithKey (\(act, _) _ -> act `elem` [farmAU, farmBU]) (sdbActivities db)}

-- | Gin B's fibre also stands in for seed from farm A.
avoiding :: SimpleDatabase -> SimpleDatabase
avoiding db = db{sdbActivities = M.adjust (\a -> a{exchanges = exchanges a ++ [avoided]}) (ginBU, fibreBU) (sdbActivities db)}
  where
    avoided = TechnosphereExchange seedU 0.5 kgU AvoidedProduct (Just farmAU) ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties Nothing

renamed :: Text -> Text -> SimpleDatabase -> SimpleDatabase
renamed from to db = db{sdbActivities = M.map (\a -> if activityName a == from then a{activityName = to} else a) (sdbActivities db)}

seedNamed :: Text -> SimpleDatabase -> SimpleDatabase
seedNamed name db = db{sdbTechFlows = M.adjust (\f -> f{tfName = name}) seedU (sdbTechFlows db)}

simple :: [((UUID, UUID), Activity)] -> [(UUID, Text)] -> SimpleDatabase
simple acts flows =
    SimpleDatabase
        { sdbActivities = M.fromList acts
        , sdbTechFlows = M.fromList [(u, TechnosphereFlow u name kgU M.empty Nothing Nothing) | (u, name) <- flows]
        , sdbBioFlows = M.empty
        , sdbWasteFlows = M.empty
        , sdbUnits = M.singleton kgU (Unit kgU "kg" "kg" "")
        , sdbDocumentation = noDocumentation
        , sdbParameters = M.empty
        , sdbProductSystems = []
        , sdbReferenceConvention = SignedReferences
        }

activity :: Text -> Text -> [Exchange] -> Activity
activity name location exs =
    Activity name [] [] M.empty M.empty location LocationDeclared "kg" exs M.empty Nothing Nothing Nothing noDates

reference :: UUID -> Exchange
reference flow = TechnosphereExchange flow 1.0 kgU ReferenceProduct Nothing ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties Nothing

input :: UUID -> Maybe UUID -> Exchange
input flow link = TechnosphereExchange flow 2.0 kgU Input link ClaimByProduct "" Nothing Nothing Nothing M.empty noProperties Nothing

testUUID :: Word32 -> UUID
testUUID n = UUID.fromWords n 0x4000 0x8000 1

farmAU, farmBU, ginAU, ginBU, millU, marketU, seedU, fibreAU, fibreBU, yarnU, powerU, kgU :: UUID
farmAU = testUUID 0x10
farmBU = testUUID 0x11
ginAU = testUUID 0x12
ginBU = testUUID 0x13
millU = testUUID 0x14
marketU = testUUID 0x15
seedU = testUUID 0xa0
fibreAU = testUUID 0xa1
fibreBU = testUUID 0xa2
yarnU = testUUID 0xa3
powerU = testUUID 0xa4
kgU = testUUID 0x01
