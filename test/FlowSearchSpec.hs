{-# LANGUAGE OverloadedStrings #-}

{- | What a flow search matches, and how it orders what it found.

Agribalyse 3.2 carries seven @Deltamethrin@ flows differing only by
compartment. A result that drops the sub-compartment, or an order that
interleaves the media, shows them as duplicate rows.
-}
module FlowSearchSpec (spec) where

import API.Types (FlowSearchResult (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.UUID as UUID
import Database (allFlows, filterByName, flowMatchesQuery, flowSearchFields)
import Service (FlowFilter (..), flowSearchResults)
import Test.Hspec
import Types (
    BiosphereFlow (..),
    Compartment (..),
    ExchangeKind (..),
    FlowKind (..),
    KindFilter (..),
    Medium (..),
    TechnosphereFlow (..),
    UUID,
    WasteFlow (..),
    flowKindName,
    parseKindNames,
 )

spec :: Spec
spec = do
    kindFilterSpec
    matchSpec
    filterSpec
    orderSpec

{- | A filter is not a search: it has no second page to relegate a lookalike
to, and an argument naming nothing must not empty the answer.
-}
filterSpec :: Spec
filterSpec = describe "Database.filterByName" $ do
    let carbonFlows =
            map
                (`namedFlow` [])
                [ "Carbon dioxide, fossil"
                , "Carbon dioxide, non-fossil, resource correction"
                , "Methane, fossil"
                ]
        kept query flows = map flowKindName (filterByName query flowSearchFields flows)

    it "keeps exactly the flow whose name carries the query as written" $
        -- The query splits into words like any other, but the name written
        -- out is the closest match, so the opposite flow that also holds
        -- "fossil" is not mixed into the answer.
        kept "Carbon dioxide, fossil" carbonFlows `shouldBe` ["Carbon dioxide, fossil"]

    it "keeps everything the words reach once the punctuation is dropped" $
        -- No name carries "carbon dioxide fossil" as typed, so the closest
        -- tier is the one carrying every word – both of them, the caller
        -- reading the two names it gets back.
        kept "carbon dioxide fossil" carbonFlows
            `shouldBe` ["Carbon dioxide, fossil", "Carbon dioxide, non-fossil, resource correction"]

    it "prefers a name match to a synonym match" $
        -- The curated registry gives biogenic CO2 the synonym
        -- "Carbon dioxide, non-fossil", which holds every word of the query.
        kept
            "Carbon dioxide, fossil"
            [ namedFlow "Carbon dioxide, biogenic" ["Carbon dioxide, non-fossil"]
            , namedFlow "Carbon dioxide, fossil" []
            ]
            `shouldBe` ["Carbon dioxide, fossil"]

    it "filters nothing when the argument names no word" $
        -- A blank filter is a filter naming nothing, not a search for
        -- nothing: emptying the answer would read as an empty inventory.
        map (`kept` carbonFlows) ["", " ", ", "]
            `shouldBe` replicate 3 (map flowKindName carbonFlows)

matchSpec :: Spec
matchSpec = describe "Database.flowMatchesQuery" $ do
    it "finds a name whose words the query didn't punctuate" $
        flowMatchesQuery "water fossil" waterFossil `shouldBe` True

    it "finds it whatever order the words come in" $
        flowMatchesQuery "fossil water" waterFossil `shouldBe` True

    it "still finds it when the query does punctuate" $
        flowMatchesQuery "water, fossil" waterFossil `shouldBe` True

    it "requires every word, not just one" $
        flowMatchesQuery "water fossil" (namedFlow "Water, lake" []) `shouldBe` False

    it "keeps matching inside a word, which is how chemicals are searched" $
        flowMatchesQuery "chlor" (namedFlow "Trichloroethane" []) `shouldBe` True

    it "reads the name and the synonyms as one text" $
        flowMatchesQuery "laughing dinitrogen" laughingGas `shouldBe` True

    it "matches nothing when the query holds no word at all" $
        flowMatchesQuery ", " waterFossil `shouldBe` False

    it "reads a query whose accents arrived decomposed" $
        -- Pasting from another application can deliver NFD, where the accent
        -- is a separate combining mark (here U+0301 after a plain "e").
        -- Splitting on it would search for "pe" and "trole" instead of the
        -- word the user typed.
        flowMatchesQuery "Pe\769trole" (namedFlow "Pétrole brut" []) `shouldBe` True

    it "searches a punctuated name for the pieces the user typed" $
        flowMatchesQuery "2,4-D" (namedFlow "2,4-D" []) `shouldBe` True

    it "looks in every family of flows, not only the biosphere" $
        map flowKindName (allFlows techMap bioMap wasteMap)
            `shouldMatchList` ["tap water", "Water, fossil", "Waste paperboard"]

waterFossil :: FlowKind
waterFossil = namedFlow "Water, fossil" []

laughingGas :: FlowKind
laughingGas = namedFlow "Dinitrogen monoxide" ["laughing gas", "nitrous oxide"]

namedFlow :: Text -> [Text] -> FlowKind
namedFlow name syns = BioKind (biosphere 0 name syns)

techMap :: M.Map UUID TechnosphereFlow
techMap = M.singleton (mkUUID 1) (TechnosphereFlow (mkUUID 1) "tap water" (mkUUID 0) M.empty Nothing Nothing)

bioMap :: M.Map UUID BiosphereFlow
bioMap = M.singleton (mkUUID 2) (biosphere 2 "Water, fossil" [])

wasteMap :: M.Map UUID WasteFlow
wasteMap = M.singleton (mkUUID 3) (WasteFlow (mkUUID 3) "Waste paperboard" (mkUUID 0) M.empty Nothing Nothing)

orderSpec :: Spec
orderSpec = describe "Service.flowSearchResults" $ do
    it "carries the sub-compartment that tells two same-medium flows apart" $
        map (\r -> (fsrCategory r, fsrCompartment r)) (search sortByName deltamethrins)
            `shouldContain` [("soil", Just "agricultural"), ("soil", Just "forestry")]

    it "groups homonyms by compartment instead of leaving them in database order" $
        -- Sorting on the name alone is stable, so seven identical names would
        -- keep the input (UUID) order: soil, water, air, soil, soil, water, air.
        map compartmentOf (search sortByName deltamethrins)
            `shouldBe` [ ("air", Just "low. pop.")
                       , ("air", Just "low. pop., long-term")
                       , ("soil", Nothing)
                       , ("soil", Just "agricultural")
                       , ("soil", Just "forestry")
                       , ("water", Just "groundwater")
                       , ("water", Just "river")
                       ]

    it "says which of the three kinds each flow is" $
        map (\r -> (fsrName r, fsrKind r)) (search byName threeKinds)
            `shouldBe` [ ("Tap water", KindTechnosphere)
                       , ("Waste water", KindWaste)
                       , ("Water, fossil", KindBiosphere)
                       ]

    it "keeps the one kind asked for" $
        map fsrName (search byName{ffKind = OnlyKinds (KindBiosphere :| [])} threeKinds)
            `shouldBe` ["Water, fossil"]

    it "keeps every kind asked for, which is how the third search tab is listed" $
        -- Biosphere and waste together: what is exchanged with nature or
        -- discarded, the bucket the search counts report as one number.
        map fsrName (search byName{ffKind = OnlyKinds (KindBiosphere :| [KindWaste])} threeKinds)
            `shouldBe` ["Waste water", "Water, fossil"]

    it "keeps all three when no kind is asked for" $
        length (search byName threeKinds) `shouldBe` 3

    it "reverses the whole order on desc, not just the requested column" $
        map compartmentOf (search sortByName{ffOrder = Just "desc"} deltamethrins)
            `shouldBe` reverse (map compartmentOf (search sortByName deltamethrins))

    it "orders by medium then sub-compartment when sorting on the category" $
        map compartmentOf (search sortByName{ffSort = Just "category"} deltamethrins)
            `shouldBe` map compartmentOf (search sortByName deltamethrins)

    it "puts the flow the user typed first, ahead of what the words also reached" $
        -- "Crude oil, in ground" is alphabetically first and is only reached
        -- because the query was split into words, so it goes last; the two
        -- names carrying "oil, crude" as typed come first, in name order.
        map fsrName (search crudeOil oilFlows)
            `shouldBe` ["Oil, crude", "Palm oil, crude, at plant", "Crude oil, in ground"]

    it "leaves the order alone when a column was asked for" $
        map fsrName (search crudeOil{ffSort = Just "name"} oilFlows)
            `shouldBe` ["Crude oil, in ground", "Oil, crude", "Palm oil, crude, at plant"]

-- | Sorted by name, so the three kinds come out in an order the test can state.
byName :: FlowFilter
byName = sortByName{ffQuery = "water", ffSort = Just "name"}

{- | One flow of each kind, all three named after water so that one query
would reach them all.
-}
threeKinds :: [FlowKind]
threeKinds =
    [ TechKind (TechnosphereFlow (mkUUID 1) "Tap water" (mkUUID 0) M.empty Nothing Nothing)
    , BioKind (biosphere 2 "Water, fossil" [])
    , WasteKind (WasteFlow (mkUUID 3) "Waste water" (mkUUID 0) M.empty Nothing Nothing)
    ]

crudeOil :: FlowFilter
crudeOil = sortByName{ffQuery = "oil, crude"}

oilFlows :: [FlowKind]
oilFlows =
    map
        (`namedFlow` [])
        ["Crude oil, in ground", "Oil, crude", "Palm oil, crude, at plant"]

compartmentOf :: FlowSearchResult -> (Text, Maybe Text)
compartmentOf r = (fsrCategory r, fsrCompartment r)

-- | No unit database: every flow reports the same unit, so unit never breaks a tie.
search :: FlowFilter -> [FlowKind] -> [FlowSearchResult]
search = flowSearchResults M.empty (const Nothing)

sortByName :: FlowFilter
sortByName =
    FlowFilter
        { ffQuery = "Deltamethrin"
        , ffLang = Nothing
        , ffKind = AnyKind
        , ffLimit = Nothing
        , ffOffset = Nothing
        , ffSort = Nothing
        , ffOrder = Nothing
        }

{- | The seven Agribalyse 3.2 flows, in the UUID order the database yields
them – deliberately not the order they should come out in.
-}
deltamethrins :: [FlowKind]
deltamethrins =
    [ bioFlow 1 Soil Nothing
    , bioFlow 2 Water (Just "groundwater")
    , bioFlow 3 Air (Just "low. pop.")
    , bioFlow 4 Soil (Just "agricultural")
    , bioFlow 5 Soil (Just "forestry")
    , bioFlow 6 Water (Just "river")
    , bioFlow 7 Air (Just "low. pop., long-term")
    ]

bioFlow :: Int -> Medium -> Maybe Text -> FlowKind
bioFlow n medium sub =
    BioKind (biosphere n "Deltamethrin" []){bfCompartment = Just (Compartment medium sub)}

biosphere :: Int -> Text -> [Text] -> BiosphereFlow
biosphere n name syns =
    BiosphereFlow
        { bfId = mkUUID n
        , bfName = name
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.singleton "en" (S.fromList syns)
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Nothing
        }

mkUUID :: Int -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

{- | What a request may name in @kind@.

A dropped name would widen the search in silence, which is the mistake the
single-kind reader was already written to avoid, so a list holding one
unreadable name is refused whole rather than filtered down to the rest.
-}
kindFilterSpec :: Spec
kindFilterSpec = describe "the kinds a request names" $ do
    it "reads one" $
        parseKindNames "biosphere" `shouldBe` Right (KindBiosphere :| [])

    it "reads several, which is how the third search tab is asked for" $
        parseKindNames "biosphere,waste" `shouldBe` Right (KindBiosphere :| [KindWaste])

    it "ignores the spaces someone writes after the commas" $
        parseKindNames " biosphere , waste " `shouldBe` Right (KindBiosphere :| [KindWaste])

    it "refuses a parameter that names no kind, rather than widening" $
        -- Someone joined an empty set of ticked boxes. Answering with every
        -- kind would label a tab with a filter nobody asked for.
        parseKindNames "" `shouldSatisfy` isLeft

    it "refuses a list of nothing but separators for the same reason" $
        parseKindNames " , " `shouldSatisfy` isLeft

    it "refuses the whole list for one name it cannot read" $
        parseKindNames "biosphere,bioshpere" `shouldSatisfy` isLeft
  where
    isLeft :: Either a b -> Bool
    isLeft = either (const True) (const False)
