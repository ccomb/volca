{-# LANGUAGE OverloadedStrings #-}

{- | Declarative, idempotent adjustments to a freshly parsed method
collection's characterization factors - the equivalent of a Brightway
import "strategy", but expressed as data ('Config.MethodPatch') instead
of an imperative function.

A patch is a pure transform of the just-parsed 'MethodCF' list: applying
the same patches to the same source file always yields the same result,
so reloading a collection never compounds an adjustment (unlike a
Brightway strategy, which mutates a persisted store and re-applies on
every re-import).
-}
module Method.Patch (
    applyMethodPatches,
    cfMatches,
    applyOp,
    describePatch,
) where

import Config (CFPatchOp (..), MethodPatch (..), MethodPatchMatch (..))
import Data.List (mapAccumL)
import Data.Maybe (maybeToList)
import qualified Data.Text as T
import Method.Types (Compartment (..), Method (..), MethodCF (..), MethodCollection (..))
import SubstanceRegistry (nonEmptyCAS)

{- | Apply every patch, in order, to a method collection. Each patch scans
every CF of every method whose category matches (via 'cfMatches') and
replaces its value with 'applyOp'; a patch with no 'mpmCategory' selector
crosses every method in the collection.

Returns the patched collection alongside, for each patch, how many CFs it
touched - a patch that touches zero is very likely a selector typo, and
the caller (which has a logging effect) is expected to surface that.
-}
applyMethodPatches :: [MethodPatch] -> MethodCollection -> (MethodCollection, [(MethodPatch, Int)])
applyMethodPatches patches collection0 = mapAccumL step collection0 patches
  where
    step collection patch =
        let (methods', touched) = patchMethods patch (mcMethods collection)
         in (collection{mcMethods = methods'}, (patch, touched))

patchMethods :: MethodPatch -> [Method] -> ([Method], Int)
patchMethods patch methods =
    let results = map (patchMethod patch) methods
     in (map fst results, sum (map snd results))

patchMethod :: MethodPatch -> Method -> (Method, Int)
patchMethod patch method =
    let category = methodName method
        go cf
            | cfMatches (mpMatch patch) category cf = (cf{mcfValue = applyOp (mpOp patch) (mcfValue cf)}, 1 :: Int)
            | otherwise = (cf, 0)
        results = map go (methodFactors method)
     in (method{methodFactors = map fst results}, sum (map snd results))

{- | Does this CF match the selector? Every field the selector sets must
match (conjunction); an unset field imposes no constraint. 'category' is
compared against the enclosing 'Method.methodName' - for a SimaPro CSV
export each impact-category section is its own 'Method' whose name is
the category (e.g. \"Resource use, fossils\"), not the collection's
overall methodology name.
-}
cfMatches :: MethodPatchMatch -> T.Text -> MethodCF -> Bool
cfMatches sel category cf =
    maybe True (== category) (mpmCategory sel)
        && maybe True (== mcfFlowName cf) (mpmFlowName sel)
        && maybe True (`T.isPrefixOf` mcfFlowName cf) (mpmFlowNamePrefix sel)
        && maybe True (casMatches (mcfCAS cf)) (mpmCAS sel)
        && maybe True (subcompartmentMatches (mcfCompartment cf)) (mpmSubcompartmentContains sel)

{- | Compare CAS numbers with both sides canonicalized, so a selector matches
whichever way the method's parser spelled the padding. A selector that states
no CAS (empty, or all zeros and dashes) matches nothing rather than matching
every CAS-less CF.
-}
casMatches :: Maybe T.Text -> T.Text -> Bool
casMatches mcfCas want = case nonEmptyCAS want of
    Nothing -> False
    Just wanted -> (nonEmptyCAS =<< mcfCas) == Just wanted

subcompartmentMatches :: Maybe Compartment -> T.Text -> Bool
subcompartmentMatches Nothing _ = False
subcompartmentMatches (Just (Compartment _ subcompartment _)) want =
    T.toLower want `T.isInfixOf` T.toLower subcompartment

applyOp :: CFPatchOp -> Double -> Double
applyOp (ScaleBy s) v = v * s
applyOp (SetValueTo v) _ = v

{- | Human-readable label for a patch, for log lines - its description when
given, else a rendering of the selector and operation.
-}
describePatch :: MethodPatch -> T.Text
describePatch patch = case mpDescription patch of
    Just d -> d
    Nothing -> describeMatch (mpMatch patch) <> " " <> describeOp (mpOp patch)

describeMatch :: MethodPatchMatch -> T.Text
describeMatch sel =
    T.intercalate ", " $
        concat
            [ ["category=" <> c | c <- maybeToList (mpmCategory sel)]
            , ["flow-name=" <> f | f <- maybeToList (mpmFlowName sel)]
            , ["flow-name-prefix=" <> f | f <- maybeToList (mpmFlowNamePrefix sel)]
            , ["cas=" <> c | c <- maybeToList (mpmCAS sel)]
            , ["subcompartment-contains=" <> s | s <- maybeToList (mpmSubcompartmentContains sel)]
            ]

describeOp :: CFPatchOp -> T.Text
describeOp (ScaleBy s) = "(scale ×" <> T.pack (show s) <> ")"
describeOp (SetValueTo v) = "(set-value " <> T.pack (show v) <> ")"
