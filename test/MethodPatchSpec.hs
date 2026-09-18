{-# LANGUAGE OverloadedStrings #-}

module MethodPatchSpec (spec) where

import Config (
    MethodPatch (..),
    MethodPatchMatch (..),
 )
import Data.Text (Text)
import qualified Data.UUID as UUID
import Method.Patch (applyMethodPatches, cfMatches)
import Method.Types (
    Compartment (..),
    FlowDirection (..),
    Method (..),
    MethodCF (..),
    MethodCollection (..),
 )
import Test.Hspec
import Types (PatchOp (..))

-- | A CF with everything defaulted, so each test only sets what it matches on.
mkCF :: Text -> Double -> MethodCF
mkCF name value =
    MethodCF
        { mcfFlowRef = UUID.nil
        , mcfFlowName = name
        , mcfDirection = Input
        , mcfValue = value
        , mcfCompartment = Nothing
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

mkMethod :: Text -> [MethodCF] -> Method
mkMethod category cfs =
    Method
        { methodId = UUID.nil
        , methodName = category
        , methodDescription = Nothing
        , methodUnit = "MJ"
        , methodCategory = category
        , methodMethodology = Just "Test methodology"
        , methodFactors = cfs
        }

-- | Every (flow name, value) pair in the collection, in method and factor order.
factorValues :: MethodCollection -> [(Text, Double)]
factorValues c = [(mcfFlowName cf, mcfValue cf) | m <- mcMethods c, cf <- methodFactors m]

emptyMatch :: MethodPatchMatch
emptyMatch =
    MethodPatchMatch
        { mpmCategory = Nothing
        , mpmFlowName = Nothing
        , mpmFlowNamePrefix = Nothing
        , mpmCAS = Nothing
        , mpmSubcompartmentContains = Nothing
        }

spec :: Spec
spec = do
    describe "cfMatches" $ do
        let cf = mkCF "Uranium" 1000

        it "matches on category + flow-name-prefix (conjunction)" $ do
            let sel = emptyMatch{mpmCategory = Just "Resource use, fossils", mpmFlowNamePrefix = Just "Uranium"}
            cfMatches sel "Resource use, fossils" cf `shouldBe` True

        it "fails when only part of the conjunction matches" $ do
            let sel = emptyMatch{mpmCategory = Just "Land use", mpmFlowNamePrefix = Just "Uranium"}
            cfMatches sel "Resource use, fossils" cf `shouldBe` False

        it "matches flow-name-prefix without requiring an exact flow-name" $ do
            let sel = emptyMatch{mpmFlowNamePrefix = Just "Uran"}
            cfMatches sel "any" cf `shouldBe` True

        it "matches subcompartment-contains case-insensitively" $ do
            let cfWithComp = cf{mcfCompartment = Just (Compartment "water" "Groundwater, long-term" "")}
                sel = emptyMatch{mpmSubcompartmentContains = Just "long-term"}
            cfMatches sel "any" cfWithComp `shouldBe` True

        it "never matches subcompartment-contains when the CF has no compartment" $ do
            let sel = emptyMatch{mpmSubcompartmentContains = Just "long-term"}
            cfMatches sel "any" cf `shouldBe` False

        it "matches CAS after normalizing leading zeros on both sides" $ do
            let cfWithCas = cf{mcfCAS = Just "7440-61-1"}
                sel = emptyMatch{mpmCAS = Just "007440-61-1"}
            cfMatches sel "any" cfWithCas `shouldBe` True

        -- The other direction: a parser that stores the source spelling as it
        -- came still has to meet a selector written canonically.
        it "matches a padded CF CAS against a canonical selector" $ do
            let cfWithCas = cf{mcfCAS = Just "007440-61-1"}
                sel = emptyMatch{mpmCAS = Just "7440-61-1"}
            cfMatches sel "any" cfWithCas `shouldBe` True

        it "never matches an all-zeros placeholder selector" $ do
            let cfWithCas = cf{mcfCAS = Just "7440-61-1"}
                sel = emptyMatch{mpmCAS = Just "000-00-0"}
            cfMatches sel "any" cfWithCas `shouldBe` False

    describe "applyMethodPatches" $ do
        let uranium = mkCF "Uranium" 560000
            uraniumOre = mkCF "Uranium ore, 1.11 GJ per kg" 1110
            coal = mkCF "Coal" 18
            method = mkMethod "Resource use, fossils" [uranium, uraniumOre, coal]
            collection = MethodCollection [method] [] [] []

        it "leaves the collection unchanged when there are no patches" $ do
            let (patched, stats) = applyMethodPatches [] collection
            patched `shouldBe` collection
            stats `shouldBe` []

        it "scales only the matched CFs, by patch name" $ do
            let patch =
                    MethodPatch
                        { mpDescription = Just "uraniumFRU"
                        , mpMatch = emptyMatch{mpmCategory = Just "Resource use, fossils", mpmFlowNamePrefix = Just "Uranium"}
                        , mpOp = ScaleBy 0.6
                        }
                (patched, stats) = applyMethodPatches [patch] collection
            factorValues patched `shouldBe` [("Uranium", 336000), ("Uranium ore, 1.11 GJ per kg", 666), ("Coal", 18)]
            stats `shouldBe` [(patch, 2)]

        it "sets matched CFs to a fixed value" $ do
            let patch =
                    MethodPatch
                        { mpDescription = Nothing
                        , mpMatch = emptyMatch{mpmFlowName = Just "Coal"}
                        , mpOp = SetValueTo 0
                        }
                (patched, stats) = applyMethodPatches [patch] collection
            factorValues patched `shouldBe` [("Uranium", 560000), ("Uranium ore, 1.11 GJ per kg", 1110), ("Coal", 0)]
            stats `shouldBe` [(patch, 1)]

        it "reports zero touched CFs for a selector that matches nothing" $ do
            let patch =
                    MethodPatch
                        { mpDescription = Nothing
                        , mpMatch = emptyMatch{mpmFlowName = Just "Nonexistent"}
                        , mpOp = ScaleBy 0.5
                        }
                (patched, stats) = applyMethodPatches [patch] collection
            patched `shouldBe` collection
            stats `shouldBe` [(patch, 0)]

        it "applies patches in order, each seeing the previous patch's result" $ do
            let halveEverything =
                    MethodPatch
                        { mpDescription = Nothing
                        , mpMatch = emptyMatch{mpmFlowName = Just "Uranium"}
                        , mpOp = ScaleBy 0.5
                        }
                halveAgain =
                    MethodPatch
                        { mpDescription = Nothing
                        , mpMatch = emptyMatch{mpmFlowName = Just "Uranium"}
                        , mpOp = ScaleBy 0.5
                        }
                (patched, _) = applyMethodPatches [halveEverything, halveAgain] collection
            lookup "Uranium" (factorValues patched) `shouldBe` Just 140000
