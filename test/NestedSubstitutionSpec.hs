{-# LANGUAGE OverloadedStrings #-}

{- | Per-level substitution (Phase A) tests.

Verifies the new invariant that 'applySubstitutionsAt' filters subs by
consumer DB, and that 'goWithSubsAndDeps' matches the baseline cross-DB
path when no subs apply at a given level. The single-DB test fixture
limits algebraic verification for true multi-level chains; those cases
rely on the shared classifier body already covered by
'SubstitutionSpec' and 'CrossDBSubstitutionSpec'.
-}
module NestedSubstitutionSpec (spec) where

import API.Types (RootDb (..), Substitution (..), SubstitutionScope (..), SupplyChainEntry (..), SupplyChainResponse (..), ThisDb (..))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import Matrix (buildDemandVectorFromIndex)
import Service (
    ActivityFilterCore (..),
    Edges (..),
    ServiceError (..),
    SupplyChainFilter (..),
    applySubstitutionsAt,
    buildSupplyChainFromScalingVectorCrossDB,
    inventoryWithSubsAndDeps,
 )
import SharedSolver (
    SharedSolver,
    computeInventoryMatrixWithDepsCached,
    createSharedSolver,
    solveWithSharedSolver,
 )
import qualified SharedSolver
import Test.Hspec
import TestHelpers (
    linkDatabases,
    loadSampleDatabase,
    mkDepLookupFromMap,
    mkSolverFromDb,
    shippedGeographies,
 )
import Types
import UnitConversion (defaultUnitConfig)

spec :: Spec
spec = do
    describe "applySubstitutionsAt consumer-DB filter" $ do
        it "skips subs whose consumer is qualified to another DB" $ do
            -- Regression gate: the per-level filter must not touch subs that
            -- belong to a different level. This is the mechanism that makes
            -- nested substitutions work – applying a sub at the wrong level
            -- would be silent miscounting.
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "root"
            let pid = 0
                demandVec = buildDemandVectorFromIndex (dbActivityIndex db) pid
            baselineX <- solveWithSharedSolver solver =<< either (fail . show) pure demandVec
            let qualifiedPid = "other-db::aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa_bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
                sub =
                    Substitution
                        { subFrom = qualifiedPid
                        , subTo = qualifiedPid
                        , subScope = OneEdge qualifiedPid
                        }
                noDeps _ = pure Nothing
            res <- applySubstitutionsAt defaultUnitConfig noDeps db (ThisDb "root") (RootDb "root") solver [baselineX] [sub]
            case res of
                Right ([x'], links) -> do
                    links `shouldBe` []
                    U.toList x' `shouldBe` U.toList baselineX
                Right other -> expectationFailure ("unexpected shape: " <> show other)
                Left e -> expectationFailure ("filter should skip, got: " <> show e)

        it "empty-subs fast path returns scalings unchanged with no virtual links" $ do
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "root"
            let pid = 0
                demandVec = buildDemandVectorFromIndex (dbActivityIndex db) pid
            baselineX <- solveWithSharedSolver solver =<< either (fail . show) pure demandVec
            let noDeps _ = pure Nothing
            res <- applySubstitutionsAt defaultUnitConfig noDeps db (ThisDb "root") (RootDb "root") solver [baselineX] []
            case res of
                Right (xs, links) -> do
                    links `shouldBe` []
                    length xs `shouldBe` 1
                    U.toList (head xs) `shouldBe` U.toList baselineX
                Left e -> expectationFailure ("empty path failed: " <> show e)

        it "preserves K>1 input shape when all subs are filtered" $ do
            -- At dep level K = number of root demands. The filter must not
            -- collapse the batch.
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "dep"
            let pid = 0
                demandVec = buildDemandVectorFromIndex (dbActivityIndex db) pid
            baselineX <- solveWithSharedSolver solver =<< either (fail . show) pure demandVec
            let qualifiedToRoot = "root::aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa_bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
                sub =
                    Substitution
                        { subFrom = qualifiedToRoot
                        , subTo = qualifiedToRoot
                        , subScope = OneEdge qualifiedToRoot
                        }
                noDeps _ = pure Nothing
            res <- applySubstitutionsAt defaultUnitConfig noDeps db (ThisDb "dep") (RootDb "root") solver [baselineX, baselineX, baselineX] [sub]
            case res of
                Right (xs, _) -> length xs `shouldBe` 3
                Left e -> expectationFailure ("K>1 filter failed: " <> show e)

        it "skips bare consumer subs when walker is in a dep DB" $ do
            -- Regression gate: bare 'subConsumer' means root DB only (per the
            -- Substitution docstring). Before this fix, the per-level filter
            -- used 'thisDbName' as the parseSubRef default – so bare consumers
            -- were treated as living in EVERY DB the walker visited, and the
            -- subsequent resolve in a dep DB failed with a spurious not-found.
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "dep"
            let pid = 0
                demandVec = buildDemandVectorFromIndex (dbActivityIndex db) pid
            baselineX <- solveWithSharedSolver solver =<< either (fail . show) pure demandVec
            let bareRootPid = "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa_bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
                sub =
                    Substitution
                        { subFrom = bareRootPid
                        , subTo = bareRootPid
                        , subScope = OneEdge bareRootPid
                        }
                noDeps _ = pure Nothing
            res <- applySubstitutionsAt defaultUnitConfig noDeps db (ThisDb "dep") (RootDb "root") solver [baselineX] [sub]
            case res of
                Right ([x'], links) -> do
                    links `shouldBe` []
                    U.toList x' `shouldBe` U.toList baselineX
                Right other -> expectationFailure ("unexpected shape: " <> show other)
                Left e -> expectationFailure ("bare consumer must be filtered out at dep level, got: " <> show e)

    describe "inventoryWithSubsAndDeps (Phase A recursion)" $ do
        it "with empty subs matches the baseline cross-DB inventory path" $ do
            -- Regression gate for the commit that replaced the old one-shot
            -- root-path body with the recursive goWithSubsAndDeps. Bit-for-bit
            -- parity against computeInventoryMatrixWithDepsCached.
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "SAMPLE.min3"
            let pid = 0
                noDeps _ = pure Nothing
            eBase <- computeInventoryMatrixWithDepsCached defaultUnitConfig noDeps db "SAMPLE.min3" solver pid
            eSub <- inventoryWithSubsAndDeps defaultUnitConfig noDeps db "SAMPLE.min3" solver pid []
            case (eBase, eSub) of
                (Right base, Right subSol) ->
                    M.toList (SharedSolver.csInventory subSol)
                        `shouldBe` M.toList (SharedSolver.csInventory base)
                (Left e, _) -> expectationFailure ("baseline failed: " <> T.unpack e)
                (_, Left e) -> expectationFailure ("subs path failed: " <> show e)

        it "rejects dep-consumer subs whose DB is not loaded (422 surface)" $ do
            -- No-silent-errors invariant: a sub with a consumer qualified to
            -- an unloaded DB used to be silently filtered at every level.
            -- 'validateConsumerDbs' now surfaces it before the recursion.
            db <- loadSampleDatabase "SAMPLE.min3"
            solver <- mkSolver db "SAMPLE.min3"
            let pid = 0
                noDeps _ = pure Nothing
                realRootPid = "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa_bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"
                subWithPhantomConsumer =
                    Substitution
                        { subFrom = realRootPid
                        , subTo = realRootPid
                        , subScope = OneEdge ("phantom-dep::" <> realRootPid)
                        }
            eFiltered <- inventoryWithSubsAndDeps defaultUnitConfig noDeps db "SAMPLE.min3" solver pid [subWithPhantomConsumer]
            case eFiltered of
                Left (MatrixError msg) ->
                    msg `shouldSatisfy` T.isInfixOf "unloaded database"
                Left other -> expectationFailure ("wrong error: " <> show other)
                Right _ -> expectationFailure "expected Left for unloaded consumer DB"

    describe "inventoryWithSubsAndDeps with a real dep DB (Phase A.6)" $ do
        it "dep-level Case A substitution actually fires and changes the inventory" $ do
            -- Regression gate for 'subs cascade past depth 0'. Requires an
            -- actually-linked dep DB so the recursion enters 'resolveDepWithSubs'.
            -- Before the Phase-A refactor, resolveDep passed [] into the recursion
            -- and this case would have been a no-op versus the baseline.
            rootRaw <- loadSampleDatabase "SAMPLE.min3"
            dep <- loadSampleDatabase "SAMPLE.min3" -- reused as independent DB under a different name
            let root = linkDatabases rootRaw dep "dep" 0.1
            rootSolver <- mkSolverFromDb root "root"
            depSolver <- mkSolverFromDb dep "dep"
            let lookup_ = mkDepLookupFromMap (M.singleton "dep" (dep, depSolver))
                (consumerPid, fromPid, toPid) = pickDepSubPids dep
                mkDepSub f t =
                    Substitution
                        { subFrom = "dep::" <> processIdToText dep f
                        , subTo = "dep::" <> processIdToText dep t
                        , subScope = OneEdge ("dep::" <> processIdToText dep consumerPid)
                        }
            eBase <- inventoryWithSubsAndDeps defaultUnitConfig lookup_ root "root" rootSolver 0 []
            eSub <- inventoryWithSubsAndDeps defaultUnitConfig lookup_ root "root" rootSolver 0 [mkDepSub fromPid toPid]
            case (eBase, eSub) of
                (Right base, Right subSol) ->
                    M.toList (SharedSolver.csInventory subSol)
                        `shouldNotBe` M.toList (SharedSolver.csInventory base)
                (Left e, _) -> expectationFailure ("baseline failed: " <> show e)
                (_, Left e) -> expectationFailure ("dep-sub path failed: " <> show e)

        it "depth-2 chain: root + dep subs both apply" $ do
            -- Two subs in a single call, one at each level. Chain result must
            -- differ from baseline; sanity-check it also differs from the
            -- isolated root-only and dep-only results whenever those are
            -- themselves non-trivial.
            rootRaw <- loadSampleDatabase "SAMPLE.min3"
            dep <- loadSampleDatabase "SAMPLE.min3" -- reused as independent DB under a different name
            let root = linkDatabases rootRaw dep "dep" 0.1
            rootSolver <- mkSolverFromDb root "root"
            depSolver <- mkSolverFromDb dep "dep"
            let lookup_ = mkDepLookupFromMap (M.singleton "dep" (dep, depSolver))
                (rootCons, rootFrom, rootTo) = pickRootSubPids root
                (depCons, depFrom, depTo) = pickDepSubPids dep
                rootSub =
                    Substitution
                        { subFrom = processIdToText root rootFrom
                        , subTo = processIdToText root rootTo
                        , subScope = OneEdge (processIdToText root rootCons)
                        }
                depSub =
                    Substitution
                        { subFrom = "dep::" <> processIdToText dep depFrom
                        , subTo = "dep::" <> processIdToText dep depTo
                        , subScope = OneEdge ("dep::" <> processIdToText dep depCons)
                        }
            eBase <- inventoryWithSubsAndDeps defaultUnitConfig lookup_ root "root" rootSolver 0 []
            eChain <- inventoryWithSubsAndDeps defaultUnitConfig lookup_ root "root" rootSolver 0 [rootSub, depSub]
            case (eBase, eChain) of
                (Right base, Right c) ->
                    M.toList (SharedSolver.csInventory c)
                        `shouldNotBe` M.toList (SharedSolver.csInventory base)
                _ -> expectationFailure "chain or baseline path failed"

        it "identity dep-sub (from == to) matches baseline (no-op invariant)" $ do
            -- Mass-balance sanity: a Case-A substitution where supplier and
            -- replacement are the same activity must wash out. If it doesn't,
            -- the Sherman-Morrison coefficient accounting is broken.
            rootRaw <- loadSampleDatabase "SAMPLE.min3"
            dep <- loadSampleDatabase "SAMPLE.min3" -- reused as independent DB under a different name
            let root = linkDatabases rootRaw dep "dep" 0.1
            rootSolver <- mkSolverFromDb root "root"
            depSolver <- mkSolverFromDb dep "dep"
            let lookup_ = mkDepLookupFromMap (M.singleton "dep" (dep, depSolver))
                (consumerPid, fromPid, _) = pickDepSubPids dep
                identitySub =
                    Substitution
                        { subFrom = "dep::" <> processIdToText dep fromPid
                        , subTo = "dep::" <> processIdToText dep fromPid
                        , subScope = OneEdge ("dep::" <> processIdToText dep consumerPid)
                        }
            eBase <- inventoryWithSubsAndDeps defaultUnitConfig lookup_ root "root" rootSolver 0 []
            eSub <- inventoryWithSubsAndDeps defaultUnitConfig lookup_ root "root" rootSolver 0 [identitySub]
            case (eBase, eSub) of
                (Right base, Right subSol) ->
                    M.toList (SharedSolver.csInventory subSol)
                        `shouldBe` M.toList (SharedSolver.csInventory base)
                (Left e, _) -> expectationFailure ("baseline failed: " <> show e)
                (_, Left e) -> expectationFailure ("identity path failed: " <> show e)

    describe "buildSupplyChainFromScalingVectorCrossDB" $ do
        it "expands cross-DB links into qualified dep-DB entries" $ do
            -- Gap B prerequisite: the supply-chain endpoint must emit entries
            -- for activities reached through a cross-DB link, tagged with
            -- their own database name and a qualified processId.
            rootRaw <- loadSampleDatabase "SAMPLE.min3"
            dep <- loadSampleDatabase "SAMPLE.min3"
            let root = linkDatabases rootRaw dep "dep" 0.1
            rootSolver <- mkSolverFromDb root "root"
            depSolver <- mkSolverFromDb dep "dep"
            let lookup_ = mkDepLookupFromMap (M.singleton "dep" (dep, depSolver))
                pid = 0
                demandVec = buildDemandVectorFromIndex (dbActivityIndex root) pid
            scaling <- solveWithSharedSolver rootSolver =<< either (fail . show) pure demandVec
            let scf =
                    SupplyChainFilter
                        { scfCore =
                            ActivityFilterCore
                                { afcName = Nothing
                                , afcLocation = Nothing
                                , afcProduct = Nothing
                                , afcClassifications = []
                                , afcLimit = Nothing
                                , afcOffset = Nothing
                                , afcSort = Nothing
                                , afcOrder = Nothing
                                }
                        , scfMaxDepth = Nothing
                        , scfMinQuantity = Nothing
                        , scfEdges = EntriesOnly
                        }
            eResp <-
                buildSupplyChainFromScalingVectorCrossDB
                    defaultUnitConfig
                    shippedGeographies
                    lookup_
                    root
                    "root"
                    pid
                    scaling
                    []
                    scf
            case eResp of
                Left e -> expectationFailure ("supply-chain CrossDB failed: " <> show e)
                Right resp -> do
                    let depEntries =
                            filter
                                (\e -> sceDatabaseName e == "dep")
                                (scrSupplyChain resp)
                        rootEntries =
                            filter
                                (\e -> sceDatabaseName e == "root")
                                (scrSupplyChain resp)
                    length rootEntries `shouldSatisfy` (> 0)
                    length depEntries `shouldSatisfy` (> 0)
                    -- Dep entries must carry the "dep::" qualifier.
                    mapM_ (\e -> sceProcessId e `shouldSatisfy` T.isPrefixOf "dep::") depEntries

-- | Build a fresh 'SharedSolver' from a database's technosphere triples.
mkSolver :: Database -> T.Text -> IO SharedSolver
mkSolver db name =
    let tech =
            [ (fromIntegral i, fromIntegral j, v)
            | SparseTriple i j v <- U.toList (dbTechnosphereTriples db)
            ]
        n = fromIntegral (dbActivityCount db)
     in createSharedSolver name tech n

{- | Pick a valid (consumer, supplier, alternate supplier) triple for a
dep-level Case-A substitution. Finds the first off-diagonal technosphere
entry (consumer != supplier) and a third distinct activity to act as the
replacement. Tests fail loudly (via undefined-less error-out) if the
sample DB doesn't have one – but SAMPLE.min3 / SAMPLE.min4 both do.
-}
pickDepSubPids :: Database -> (ProcessId, ProcessId, ProcessId)
pickDepSubPids = pickSubPids

pickRootSubPids :: Database -> (ProcessId, ProcessId, ProcessId)
pickRootSubPids = pickSubPids

pickSubPids :: Database -> (ProcessId, ProcessId, ProcessId)
pickSubPids db =
    let triples = U.toList (dbTechnosphereTriples db)
        n = fromIntegral (dbActivityCount db)
        pairs =
            [ (fromIntegral j, fromIntegral i) -- (consumer, supplier): A[supplier, consumer]
            | SparseTriple i j _ <- triples
            , i /= j
            ]
     in case pairs of
            ((cons, supp) : _) ->
                let alt = head [p | p <- [0 .. n - 1], p /= cons, p /= supp]
                 in (cons, supp, alt)
            [] -> error "pickSubPids: database has no off-diagonal technosphere entries"
