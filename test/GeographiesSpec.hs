{-# LANGUAGE OverloadedStrings #-}

{- | Regression gate for the geography cascade defined in
@data/geographies.csv@. The cascade is the chain of fallback locations
that the LCIA matcher walks when a CF for the requested location is
absent – a missing edge silently demotes a location to the legacy
global behavior.
-}
module GeographiesSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Database.CrossLinking (acceptableLocation)
import Database.Manager (hierarchyFromGeographies, parseGeographiesCSV)
import Method.Types (Location (..))
import Test.Hspec
import Types (GeographyPolicy (..), LocationKind (..))

table :: IO (M.Map Text (Text, [Text]))
table = do
    parsed <- parseGeographiesCSV "data/geographies.csv"
    case parsed of
        Right geos -> pure geos
        Left err -> fail (T.unpack err)

{- | The table in the shape the matcher reads it, through the engine's own
derivation rather than a copy of it.
-}
hierarchy :: IO (M.Map Location [Location])
hierarchy = hierarchyFromGeographies <$> table

spec :: Spec
spec = do
    describe "data/geographies.csv cascade" $ do
        it "RoW cascades to GLO (commit 7df7921)" $ do
            -- Before this fix, RoW had an empty parents column, so any
            -- "Rest of World" CF lookup failed instead of falling back to
            -- the global value – silently underestimating impact for every
            -- ecoinvent activity whose location is RoW.
            geos <- table
            case M.lookup "RoW" geos of
                Just (display, parents) -> do
                    display `shouldBe` "Rest of World"
                    parents `shouldBe` ["GLO"]
                Nothing -> expectationFailure "RoW missing from geographies.csv"

        it "GLO cascades to RoW (existing fallback, regression gate)" $ do
            geos <- table
            case M.lookup "GLO" geos of
                Just (_, parents) -> parents `shouldBe` ["RoW"]
                Nothing -> expectationFailure "GLO missing from geographies.csv"

        it "every non-leaf entry has at least one parent" $ do
            -- Sanity check that no other row regresses to the empty-parents
            -- state RoW used to have. The only leaves are GLO and RoW
            -- themselves (mutual fallback between the two).
            geos <- table
            let orphaned =
                    [ code
                    | (code, (_, parents)) <- M.toList geos
                    , null parents
                    ]
            orphaned `shouldBe` []

        it "every parent is itself a row" $ do
            -- A parent nobody defines ends the cascade one step early and
            -- without saying so: the lookup for that step finds nothing and
            -- the walk stops, short of GLO.
            geos <- table
            let dangling =
                    [ (code, parent)
                    | (code, (_, parents)) <- M.toList geos
                    , parent <- parents
                    , not (M.member parent geos)
                    ]
            dangling `shouldBe` []

        it "no location is its own ancestor" $ do
            -- A cycle through the parent lists makes "wider than"
            -- meaningless: two locations on it would each be accepted as a
            -- fallback for the other, in both directions. Checked
            -- transitively, not just for mutual pairs. GLO and RoW are the
            -- deliberate exception – they are the same breadth.
            geos <- table
            let parentsOf c = maybe [] snd (M.lookup c geos)
                ancestors c = go [] (parentsOf c)
                  where
                    go seen [] = seen
                    go seen (p : ps)
                        | p `elem` seen = go seen ps
                        | otherwise = go (p : seen) (parentsOf p ++ ps)
                cyclic =
                    [ code
                    | code <- M.keys geos
                    , code `notElem` ["GLO", "RoW"]
                    , code `elem` ancestors code
                    ]
            cyclic `shouldBe` []

        it "GLO and RoW close every parents list" $ do
            -- The regionalized CF cascade walks the parents in order and
            -- stops at the first factor it finds, so a parent listed after
            -- the global codes could never beat the global average – exactly
            -- the fallback the nearer parents exist to improve on.
            geos <- table
            let placeless = ["GLO", "RoW"] :: [Text]
                misordered =
                    [ code
                    | (code, (_, parents)) <- M.toList geos
                    , any (`notElem` placeless) (dropWhile (`notElem` placeless) parents)
                    ]
            misordered `shouldBe` []

        it "keeps a code that contains a comma in one piece" $ do
            -- "Europe, Western" is one location, not a code called "Europe"
            -- with a stray field after it. Splitting the line on commas used
            -- to cut it in half, which is why the file goes through a real
            -- CSV reader.
            geos <- table
            case M.lookup "Europe, Western" geos of
                Just (display, parents) -> do
                    display `shouldBe` "Western Europe"
                    parents `shouldContain` ["Europe"]
                Nothing -> expectationFailure "\"Europe, Western\" missing from geographies.csv"

        it "puts a country in its UN subregion, not just its continent" $ do
            -- The subregion memberships are what make a regional dataset
            -- usable for a country: without them a French activity can only
            -- fall back to Europe, and from there straight to the global
            -- average.
            geos <- table
            case M.lookup "FR" geos of
                Just (_, parents) -> parents `shouldContain` ["Europe, Western"]
                Nothing -> expectationFailure "FR missing from geographies.csv"

    describe "what the cascade lets the matcher accept" $ do
        it "offers a Western European dataset to a French activity" $ do
            -- The point of the table, stated as behaviour rather than as
            -- rows: a location the file does not know is Unrelated, which
            -- only the loosest policy accepts and only as a global caveat.
            hier <- hierarchy
            acceptableLocation GeoParent hier (Location "FR") (Location "Europe, Western")
                `shouldBe` Just ParentLoc

        it "refuses to pass a French dataset off as Western European" $ do
            -- The other direction is narrowing: France is one member of the
            -- region, so its data must not stand in for the whole of it.
            hier <- hierarchy
            acceptableLocation GeoParent hier (Location "Europe, Western") (Location "FR")
                `shouldBe` Nothing

        it "offers Brazil to one of its states" $ do
            hier <- hierarchy
            acceptableLocation GeoParent hier (Location "BR-MG") (Location "BR")
                `shouldBe` Just ParentLoc

        it "does not offer one Brazilian state to another" $ do
            hier <- hierarchy
            acceptableLocation GeoParent hier (Location "BR-MG") (Location "BR-SP")
                `shouldBe` Nothing

        it "offers the country-plus-territories aggregate to the bare country" $ do
            -- Same shape as the Canada / "Canada without Quebec" correction:
            -- "France, including overseas territories" is wider than FR, so
            -- the aggregate is a fallback for France – never the reverse.
            hier <- hierarchy
            acceptableLocation GeoParent hier (Location "FR") (Location "France, including overseas territories")
                `shouldBe` Just ParentLoc

        it "refuses bare-country data for the country-plus-territories aggregate" $ do
            hier <- hierarchy
            acceptableLocation GeoParent hier (Location "France, including overseas territories") (Location "FR")
                `shouldBe` Nothing
