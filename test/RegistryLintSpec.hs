{-# LANGUAGE OverloadedStrings #-}

{- | Offline lint of the curated flow-name registry (@data/flows.csv@).

The registry asserts substance identities that the LCIA matcher applies
blindly, so these checks constrain what the file may SAY, at commit time -
a bad bridge fails CI here instead of silently corrupting scores:

* every equivalence class stays plausibly small (a large closure means an
  ambiguous bridge fused unrelated substances - the junk-hub failure mode);
* no class fuses distinct carbon-origin qualifiers (fossil, biogenic,
  land-use-change) - those are different flows with different CFs by design;
* CAS metadata, where provided, is well-formed (check digit) and consistent
  within a class (one class = one substance = one CAS).
-}
module RegistryLintSpec (spec) where

import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit, ord)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import SubstanceRegistry (normalizeCAS)
import SynonymDB (
    BridgeDirection (..),
    RegistryRow (..),
    SynEdge (..),
    SynonymDB (..),
    buildFromEdges,
    lookupSynonymGroup,
    normalizeName,
    parseRegistryCSV,
 )

{- | An accidental transitive fusion shows up as a class an order of magnitude
larger than curation produces (largest legitimate class today: 7 names).
-}
maxClassSize :: Int
maxClassSize = 12

spec :: Spec
spec = do
    describe "curated registry lint (data/flows.csv)" $ do
        parsed <- runIO (parseRegistryCSV <$> BL.readFile "data/flows.csv")
        case parsed of
            Left err -> it "parses" $ expectationFailure err
            Right rows -> do
                let db = buildFromEdges (map rrEdge rows)
                    classes = M.elems (synIdToNames db)
                    classOf = lookupSynonymGroup db

                it "keeps every equivalence class plausibly small" $
                    filter ((> maxClassSize) . length) classes `shouldBe` []

                it "never fuses distinct carbon-origin qualifiers into one class" $
                    filter ((> 1) . S.size . originQualifiers) classes `shouldBe` []

                -- A parametric energy-resource variant ("Coal, 26.4 MJ per kg")
                -- must reach its family's energy CF through the closure; the
                -- variant's own calorific value is parsed from its name at
                -- conversion time, so sharing the family class is safe. Coal is
                -- the one family split in two (hard/brown), so each variant must
                -- land in the right half - and only there.
                it "bridges every parametric coal variant into its family's energy chain" $ do
                    let hard = classOf "Energy, from coal"
                        brown = classOf "Energy, from coal, brown"
                    hard `shouldNotBe` Nothing
                    brown `shouldNotBe` Nothing
                    classOf "Coal, 18 MJ per kg" `shouldBe` hard
                    classOf "Coal, 26.4 MJ per kg" `shouldBe` hard
                    classOf "Coal, 29.3 MJ per kg" `shouldBe` hard
                    classOf "Coal, hard" `shouldBe` hard
                    classOf "Coal, brown, 8 MJ per kg" `shouldBe` brown
                    classOf "Coal, brown, 10 MJ per kg" `shouldBe` brown

                it "keeps hard and brown coal in separate classes" $
                    classOf "Energy, from coal" == classOf "Energy, from coal, brown"
                        `shouldBe` False

                it "has well-formed CAS numbers (format + check digit)" $
                    [c | r <- rows, Just c <- [rrCas r], not (casValid c)] `shouldBe` []

                it "gives every equivalence class at most one CAS" $
                    filter ((> 1) . S.size . classCas rows) classes `shouldBe` []

    -- The checks above assert the SHIPPED registry passes, so a helper that
    -- degenerates to always-empty would keep them green. These fixtures prove
    -- each helper still fires on data it must reject.
    describe "registry lint helpers (negative controls)" $ do
        it "detects two carbon-origin families fused in one class" $
            originQualifiers ["carbon dioxide fossil", "carbon dioxide biogenic"]
                `shouldBe` S.fromList ["fossil", "biogenic"]

        it "lets an unqualified name coexist with a single family" $
            originQualifiers ["carbon dioxide", "carbon dioxide land transformation"]
                `shouldBe` S.singleton "luluc"

        it "rejects a CAS with a wrong check digit" $
            casValid "75-69-5" `shouldBe` False

        it "rejects malformed CAS shapes" $
            filter casValid ["", "75694", "7-69-4", "75-6-4", "75-69-44", "7x-69-4", "12345678-69-4"]
                `shouldBe` []

        it "accepts a valid CAS" $
            casValid "75-69-4" `shouldBe` True

        it "counts a zero-padded and a canonical spelling of one CAS as one" $ do
            let row n cas = RegistryRow (SynEdge n "peer" BridgeBoth) (Just cas) Nothing
                rows = [row "alpha" "075-69-4", row "beta" "75-69-4"]
            classCas rows ["alpha", "beta"] `shouldBe` S.singleton "75-69-4"

{- | Carbon-origin families named inside a class. A name with no qualifier is
compatible with any single family (SimaPro's bare \"Carbon dioxide\" IS the
fossil flow), but two DIFFERENT families in one class means the registry
equates flows that characterization methods deliberately split.
Names arrive normalized (lowercased, punctuation stripped).
-}
originQualifiers :: [Text] -> S.Set Text
originQualifiers = S.fromList . concatMap families
  where
    families n =
        ["fossil" | "fossil" `T.isInfixOf` n, not ("non-fossil" `T.isInfixOf` n)]
            <> ["biogenic" | "biogenic" `T.isInfixOf` n || "non-fossil" `T.isInfixOf` n]
            <> ["luluc" | any (`T.isInfixOf` n) lulucPhrases]
    lulucPhrases =
        ["land transformation", "land use change", "peat oxidation", "soil or biomass stock"]

{- | CAS numbers declared for a class's member names, in canonical form
('normalizeCAS' strips ecoinvent-style zero-padding, so padded spellings of one
substance count as one CAS). A row's CAS applies to both endpoints of its
bridge (the bridge asserts one substance), keyed by normalized name to line up
with the class members.
-}
classCas :: [RegistryRow] -> [Text] -> S.Set Text
classCas rows cls = S.unions (mapMaybe (`M.lookup` byName) cls)
  where
    byName =
        M.fromListWith
            S.union
            [ (n, S.singleton (normalizeCAS cas))
            | r <- rows
            , Just cas <- [rrCas r]
            , n <- [normalizeName (seA (rrEdge r)), normalizeName (seB (rrEdge r))]
            ]

{- | CAS format @NNNNNNN-NN-N@ (2-7 digits, 2 digits, check digit): the check
digit is the mod-10 weighted sum of the other digits, weighted 1.. from the
right. Catches transcription typos at commit time.
-}
casValid :: Text -> Bool
casValid t = case traverse digits (T.splitOn "-" t) of
    Just [body, mid, [check]] ->
        between 2 7 (length body)
            && length mid == 2
            && sum (zipWith (*) [1 ..] (reverse (body <> mid))) `mod` 10 == check
    _ -> False
  where
    digits = traverse digitVal . T.unpack
    digitVal ch = if isDigit ch then Just (ord ch - ord '0') else Nothing
    between lo hi n = lo <= n && n <= hi
