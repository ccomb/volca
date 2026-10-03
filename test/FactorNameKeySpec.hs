{-# LANGUAGE OverloadedStrings #-}

{- | Which name a factor is filed under in the exact-name table: the database
flow's name when the line was attached by name, synonym or proxy, and the
method's own name when it was attached by UUID or CAS, or by nothing.
-}
module FactorNameKeySpec (spec) where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Test.Hspec

import Method.Mapping (MatchStrategy (..), MethodTables (..), Resolution (..), buildMethodTables)
import Method.Types (Compartment (..), FlowDirection (..), MethodCF (..))
import qualified SubstanceRegistry as SR
import Types (BiosphereFlow (..), Medium (..))
import qualified Types as VT

mkUUID :: Integer -> UUID
mkUUID n = UUID.fromWords64 (fromIntegral n) 0

-- The method spells the substance one way, the database another.
methodLine :: MethodCF
methodLine =
    MethodCF
        { mcfFlowRef = mkUUID 100
        , mcfFlowName = "Dinitrogen monoxide"
        , mcfDirection = Output
        , mcfValue = 273
        , mcfCompartment = Just (Compartment "air" "" "")
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

databaseFlow :: BiosphereFlow
databaseFlow =
    BiosphereFlow
        { bfId = mkUUID 1
        , bfName = "Nitrous oxide"
        , bfUnitId = mkUUID 0
        , bfSynonyms = M.empty
        , bfCAS = Nothing
        , bfSubstanceId = Nothing
        , bfCompartment = Just (VT.Compartment Air Nothing)
        }

-- | The names the exact-name table files the line under, once attached by this strategy.
keyedUnder :: Maybe MatchStrategy -> [Text]
keyedUnder strategy =
    [ n
    | (SR.NormName n, _, _) <-
        M.keys (mtExactCF (buildMethodTables mempty mempty M.empty [(methodLine, fmap (Resolution databaseFlow) strategy)]))
    ]

spec :: Spec
spec = describe "the exact-name key of a method line" $ do
    it "is the database flow's name when the line was attached by name, synonym or proxy" $
        map (keyedUnder . Just) [ByName, BySynonym, ByProxy]
            `shouldBe` replicate 3 ["nitrous oxide"]
    it "is the method's own name when the line was attached by UUID or CAS" $
        map (keyedUnder . Just) [ByUUID, ByCAS]
            `shouldBe` replicate 2 ["dinitrogen monoxide"]
    it "is the method's own name when no database flow claimed the line" $
        keyedUnder Nothing `shouldBe` ["dinitrogen monoxide"]
