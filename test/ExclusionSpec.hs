{-# LANGUAGE OverloadedStrings #-}

{- | What a search leaves out, on top of what it asks for.

Six processes of a made-up database: two whose name holds @CIQUAL@, two whose
source identifier (not their name) holds @ZEBRA@, and a place table putting
France inside the region @EUR@. The steel sheet is what the other five buy.
Each test names the rows that must come back, in the order a listing gives
them (by name), so a row removed too often or too seldom, or a list reordered,
shows.
-}
module ExclusionSpec (spec) where

import Control.Monad (forM_)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import API.Types (ConsumerResult (..), ConsumersResponse (..), SearchResults (..), SupplyChainEntry (..))
import Database (Geographies, buildDatabaseWithMatrices, readGeographies)
import Method.Types (Location (..))
import Search.BM25 (addBM25Index)
import Service (
    ActivityFilterCore (..),
    Collected (..),
    ConsumerFilter (..),
    Edges (..),
    Exclusions (..),
    SearchFilter (..),
    SortOrder (..),
    SupplyChainFilter (..),
    WalkLevel (..),
    activitiesNamed,
    activityMatches,
    collectSupplyChainEntries,
    getConsumers,
    noExclusions,
 )
import TestHelpers (mkActivity, mkTechFlow, reference, techInput, units)
import Types (
    Activity (..),
    BuildInputs (..),
    ClassificationFilter (..),
    ClassificationMatch (..),
    Database (..),
    Exchange (..),
    FileReading (..),
    NativeProcessId (..),
    ProcessId,
    ReferenceConvention (..),
    SimpleDatabase (..),
    UUID,
    findProcessId,
    noDocumentation,
    processIdToText,
 )
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = beforeAll fixture $ describe "Activity filter exclusions" $ do
    describe "of a search" $ do
        it "a name exclusion removes what that name alone finds, identifier hits included" $ \db -> do
            -- "steel" is in a name, "zebra000123" is only the identifier of one process
            found db (noFilter{afcExcluded = leaving{exNames = ["steel", "zebra000123"]}})
                `shouldBe` ["Bottle glass", "Cheese CIQUAL", "Film plastic", "Wheat CIQUAL"]
            -- a fragment of an identifier is read the way a search reads one
            found db (noFilter{afcExcluded = leaving{exNames = ["zebra"]}})
                `shouldBe` ["Bottle glass", "Cheese CIQUAL", "Film plastic", "Wheat CIQUAL"]

        it "a name exclusion is case-blind" $ \db ->
            found db (noFilter{afcExcluded = leaving{exNames = ["ciqual"]}})
                `shouldBe` ["Bottle glass", "Film plastic", "Plain bread", "Steel sheet"]

        it "a name exclusion also removes rows a name search found by identifier" $ \db ->
            -- both processes answer "zebra" by identifier; the second is also left out by name
            found db (noFilter{afcName = Just "zebra", afcExcluded = leaving{exNames = ["steel"]}})
                `shouldBe` ["Plain bread"]

        it "a name exclusion leaves a ranked name search in its ranking" $ \db -> do
            let ranked = found (addBM25Index db) (noFilter{afcName = Just "ciqual"})
            ranked `shouldMatchList` ["Cheese CIQUAL", "Wheat CIQUAL"]
            found (addBM25Index db) (noFilter{afcName = Just "ciqual", afcExcluded = leaving{exNames = ["cheese"]}})
                `shouldBe` filter (/= "Cheese CIQUAL") ranked

        it "a location exclusion removes every place the region answers for" $ \db ->
            found db (noFilter{afcExcluded = leaving{exLocations = ["EUR"]}})
                `shouldBe` ["Cheese CIQUAL", "Film plastic", "Steel sheet"]

        it "a location exclusion is case-blind" $ \db ->
            found db (noFilter{afcExcluded = leaving{exLocations = ["fr"]}})
                `shouldBe` ["Cheese CIQUAL", "Film plastic", "Plain bread", "Steel sheet"]

        it "an exact search reads an excluded place as that place alone" $ \db ->
            foundExactly db (noFilter{afcExcluded = leaving{exLocations = ["EUR"]}})
                `shouldBe` ["Bottle glass", "Cheese CIQUAL", "Film plastic", "Steel sheet", "Wheat CIQUAL"]

        it "classification exclusions all apply, even on one system" $ \db ->
            found db (noFilter{afcExcluded = leaving{exClassifications = [category MatchContains "Packaging", category MatchExact "Agricultural/Cereal"]}})
                `shouldBe` ["Cheese CIQUAL", "Steel sheet"]

        it "positive classifications on one system stay alternatives" $ \db ->
            found db (noFilter{afcClassifications = [category MatchContains "Agricultural", category MatchContains "Packaging"]})
                `shouldBe` ["Bottle glass", "Cheese CIQUAL", "Film plastic", "Plain bread", "Wheat CIQUAL"]

        it "exclusions that leave nothing return an empty list" $ \db ->
            found db (noFilter{afcExcluded = leaving{exLocations = ["EUR", "CH", "GLO"]}})
                `shouldBe` []

    describe "of a list of named processes" $
        it "the listing of named processes honours every exclusion, in the order named" $ \db ->
            -- the wheat by its name, the film by its place, the bottle by its classification
            let excluded = leaving{exNames = ["wheat"], exLocations = ["GLO"], exClassifications = [category MatchExact "Packaging/Glass"]}
                listed = activitiesNamed places db (SearchFilter (noFilter{afcExcluded = excluded}) False) (map (processNumbered db) [6, 5, 4, 3, 2, 1])
             in map (activityName . snd) listed `shouldBe` ["Steel sheet", "Plain bread", "Cheese CIQUAL"]

    describe "of a walk" $
        it "supply chain and consumers honour every exclusion" $ \db ->
            forM_ [("without index", db), ("with index", addBM25Index db)] $ \(index, searched) ->
                forM_ walked $ \(kind, excluded, expected) -> do
                    let label = index <> ", " <> kind
                    (label, supplied searched excluded) `shouldBe` (label, expected)
                    (label, consumed searched excluded) `shouldBe` (label, expected)
  where
    -- What the walks reach from the steel sheet: the other five, less what each kind of exclusion removes.
    walked :: [(Text, Exclusions, [Text])]
    walked =
        [ ("names", leaving{exNames = ["ciqual"]}, ["Bottle glass", "Film plastic", "Plain bread"])
        , ("places", leaving{exLocations = ["EUR"]}, ["Cheese CIQUAL", "Film plastic"])
        , ("classifications", leaving{exClassifications = [category MatchContains "Packaging"]}, ["Cheese CIQUAL", "Plain bread", "Wheat CIQUAL"])
        ]

    leaving :: Exclusions
    leaving = noExclusions

-- | The names a search finds, in the order it lists them.
found :: Database -> ActivityFilterCore -> [Text]
found db core = names db (SearchFilter core False)

foundExactly :: Database -> ActivityFilterCore -> [Text]
foundExactly db core = names db (SearchFilter core True)

names :: Database -> SearchFilter -> [Text]
names db sf = [activityName a | (_, a) <- activityMatches places db sf]

-- | The processes a supply chain from the steel sheet lists, by name.
supplied :: Database -> Exclusions -> [Text]
supplied db excluded =
    sort
        [ sceActivityName entry
        | entry <- cEntries (collectSupplyChainEntries places db "db" (RootLevel (steel db)) (U.replicate (V.length (dbActivities db)) 1) (walking excluded))
        ]

-- | The processes buying, however far, from the steel sheet, by name.
consumed :: Database -> Exclusions -> [Text]
consumed db excluded =
    either (const []) (map crActivityName . srResults . crrResults) $
        getConsumers places db "db" (processIdToText db (steel db)) (ConsumerFilter (noFilter{afcExcluded = excluded, afcSort = Just "name"}) Nothing EntriesOnly)

steel :: Database -> ProcessId
steel db = processNumbered db 6

-- | The process the fixture numbers as given.
processNumbered :: Database -> Int -> ProcessId
processNumbered db n = fromMaybe (error "processNumbered: not in the fixture") (findProcessId db (uuid n) (uuid n))

walking :: Exclusions -> SupplyChainFilter
walking excluded =
    SupplyChainFilter
        { scfCore = noFilter{afcExcluded = excluded}
        , scfMaxDepth = Nothing
        , scfMinQuantity = Nothing
        , scfEdges = EntriesOnly
        , scfWithheld = mempty
        }

noFilter :: ActivityFilterCore
noFilter =
    ActivityFilterCore
        { afcName = Nothing
        , afcLocation = Nothing
        , afcProduct = Nothing
        , afcClassifications = []
        , afcExcluded = noExclusions
        , afcLimit = Nothing
        , afcOffset = Nothing
        , afcSort = Nothing
        , afcOrder = Forward
        }

category :: ClassificationMatch -> Text -> ClassificationFilter
category match value = ClassificationFilter{clfSystem = "Category", clfValue = value, clfMatch = match}

-- | France is inside @EUR@, Switzerland is not, and the world is nobody's container.
places :: Geographies
places =
    readGeographies $
        M.fromList
            [ (Location "FR", [Location "EUR", Location "GLO"])
            , (Location "EUR", [Location "GLO"])
            , (Location "CH", [Location "GLO"])
            ]

uuid :: Int -> UUID
uuid n = UUID.fromWords64 (fromIntegral n) 0

-- | What distinguishes one process of the fixture.
data Row = Row
    { rowName :: Text
    , rowPlace :: Text
    , rowClass :: Maybe Text
    , rowIdentifier :: Maybe Text
    }

{- | Processes 1 to 5 each buy the steel sheet, numbered 6. Every process
makes the one product its own number names, under the same number.
-}
fixture :: IO Database
fixture = do
    built <-
        buildDatabaseWithMatrices
            (BuildInputs defaultUnitConfig mempty ReadDeclared [])
            SimpleDatabase
                { sdbActivities = M.fromList [((uuid n, uuid n), process n row) | (n, row) <- rows]
                , sdbTechFlows = M.fromList [(uuid n, mkTechFlow (uuid n) (rowName row)) | (n, row) <- rows]
                , sdbBioFlows = M.empty
                , sdbWasteFlows = M.empty
                , sdbUnits = units
                , sdbDocumentation = noDocumentation
                , sdbParameters = M.empty
                , sdbProductSystems = []
                , sdbReferenceConvention = SignedReferences
                }
    either (fail . ("buildDatabaseWithMatrices: " <>) . show) pure built
  where
    rows :: [(Int, Row)]
    rows =
        zip
            [1 ..]
            [ Row "Wheat CIQUAL" "FR" (Just "Agricultural/Cereal") Nothing
            , Row "Cheese CIQUAL" "CH" (Just "Agricultural/Dairy") Nothing
            , Row "Bottle glass" "FR" (Just "Packaging/Glass") Nothing
            , Row "Film plastic" "GLO" (Just "Packaging/Plastic") Nothing
            , Row "Plain bread" "EUR" (Just "Agricultural/Cereal") (Just "ZEBRA000123")
            , Row "Steel sheet" "CH" Nothing (Just "ZEBRA000124")
            ]

    process :: Int -> Row -> Activity
    process n row =
        (mkActivity (rowName row) (output : [bought | n /= 6]))
            { activityLocation = rowPlace row
            , activityClassification = M.fromList [("Category", c) | Just c <- [rowClass row]]
            , activityNativeId = NativeProcessId <$> rowIdentifier row
            }
      where
        output :: Exchange
        output = (reference (uuid n)){techActivityLinkId = Just (uuid n), techLocation = ""}

    bought :: Exchange
    bought = (techInput (uuid 6) 0.5){techActivityLinkId = Just (uuid 6), techLocation = ""}
