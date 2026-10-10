{-# LANGUAGE OverloadedStrings #-}

{- | An openLCA package computed into a database: units converted to each
flow's reference unit, amounts evaluated from their formulas, multi-product
processes allocated by their own method, and inputs linked to a provider in
openLCA's order. What the reader cannot do or chose for the user is said, as
'Notice's, never hidden.

openLCA calls a process's products its product outputs and its waste
inputs; the engine, like EcoSpold 2, knows only product outputs, a waste
treatment's reference being a negative output of the waste. So a waste input
becomes a product output of the opposite amount.
-}
module OlcaSchema.Parser (
    parseOlcaDirectory,
    buildDatabase,
    Built (..),
    Notice (..),
    describeNotices,
) where

import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Text.Printf (printf)

import Data.Indexing (uniqueIndex)
import Database.Allocation (Allocating (..), allocateWith, exchangeShares)
import qualified Expr
import qualified OlcaSchema.Package as P
import Progress (ProgressLevel (..), reportProgress)
import Types
import UnitConversion (UnitConfig)

-- | What the load says about a package, one value per thing worth saying.
data Notice
    = -- | A formula computing another amount than the file stores; the computed one is kept.
      Divergent !Text
    | -- | A formula with no value; the stored amount is kept.
      Unevaluable !Text
    | -- | A calculated parameter with no value.
      Unsettled !Text
    | -- | A multi-product process carrying a product whole, for want of a factor or a method.
      WithoutFactor !Text
    | -- | A causal factor naming a product or a line the process does not have.
      StrayFactor !Text
    | -- | An input several processes produce, linked to the first by identifier.
      Tied !Text
    | -- | An input no process of the package produces.
      CutOff !Text
    | -- | An elementary flow whose category names no medium the engine knows.
      Unplaced !Text
    | -- | Something the package states that this reader does not read yet.
      NotRead !P.Unread
    deriving (Eq, Show)

data Built = Built
    { builtDatabase :: !SimpleDatabase
    , builtNotices :: ![Notice]
    }

-- | Read and compute a package, saying what the load noticed.
parseOlcaDirectory :: UnitConfig -> AllocationKey -> FilePath -> IO (Either Text SimpleDatabase)
parseOlcaDirectory cfg key dir = do
    reportProgress Info ("Loading openLCA package from: " <> dir)
    package <- P.readPackage dir
    case package >>= buildDatabase cfg key of
        Left why -> pure (Left why)
        Right b -> do
            mapM_ (reportProgress Warning . T.unpack) (describeNotices (builtNotices b))
            let sdb = builtDatabase b
            reportProgress Info $
                printf
                    "openLCA package loaded: %d processes, %d product flows, %d elementary flows"
                    (M.size (sdbActivities sdb))
                    (M.size (sdbTechFlows sdb))
                    (M.size (sdbBioFlows sdb))
            pure (Right sdb)

-- | What every process is read against.
data Context = Context
    { cxPackage :: !P.Package
    , cxFlowUnits :: !(M.Map UUID UUID)
    -- ^ Each flow's reference unit, for the flows that have one
    , cxProducers :: !(M.Map UUID (M.Map UUID P.ProcessType))
    -- ^ For each flow, the processes offering it, by identifier
    , cxAllocating :: !Allocating
    }

buildDatabase :: UnitConfig -> AllocationKey -> P.Package -> Either Text Built
buildDatabase cfg key pkg = case partitionEithers (map (readProcess cx) (P.pkProcesses pkg)) of
    ([], processes) -> do
        activities <- first describeRepeated (uniqueIndex (concatMap rpActivities processes))
        Right
            Built
                { builtDatabase =
                    SimpleDatabase
                        { sdbActivities = activities
                        , sdbTechFlows = ftTech tables
                        , sdbBioFlows = ftBio tables
                        , sdbWasteFlows = ftWaste tables
                        , sdbUnits = unitDB
                        , sdbDocumentation = noDocumentation
                        }
                , builtNotices = ftNotices tables <> concatMap rpNotices processes <> [NotRead u | p <- P.pkProcesses pkg, u <- P.prUnread p]
                }
    (failures, _) -> Left (describeUnreadable (concat failures))
  where
    unitDB :: UnitDB
    unitDB = referenceUnits pkg

    flowUnits :: M.Map UUID UUID
    flowUnits = M.mapMaybe (flowUnit pkg) (P.pkFlows pkg)

    tables :: FlowTables
    tables = flowTables pkg flowUnits

    cx :: Context
    cx =
        Context
            { cxPackage = pkg
            , cxFlowUnits = flowUnits
            , cxProducers = producerIndex pkg
            , cxAllocating = Allocating{alKey = key, alUnitConfig = cfg, alUnitDB = unitDB}
            }

-- | Two activities of one (process, product): a process listing its product twice, which no split can tell apart.
describeRepeated :: NE.NonEmpty (UUID, UUID) -> Text
describeRepeated repeated =
    "processes make the same product twice: " <> T.intercalate ", " [UUID.toText p <> " · " <> UUID.toText f | (p, f) <- NE.toList repeated]

-- | Why a line cannot be read, each naming the line.
data Unreadable
    = -- | Its flow is not among the package's flows.
      MissingFlow !Text
    | -- | Its flow's reference property leads to no unit group.
      NoReferenceUnit !Text
    | -- | Its unit or property has no factor to the flow's reference unit.
      NoConversion !Text

-- | Every unreadable line, grouped by why, so the whole package can be fixed in one pass.
describeUnreadable :: [Unreadable] -> Text
describeUnreadable failures =
    T.intercalate "\n" $
        (T.pack (show (length failures)) <> " lines cannot be read:")
            : concat
                [ ("  " <> T.pack (show (length named)) <> " " <> heading) : map ("    " <>) named
                | (heading, named) <- groups
                , not (null named)
                ]
  where
    groups :: [(Text, [Text])]
    groups =
        [ ("name a flow the package does not carry:", [n | MissingFlow n <- failures])
        , ("have a flow with no reference unit:", [n | NoReferenceUnit n <- failures])
        , ("are in a unit with no conversion to their flow's reference unit:", [n | NoConversion n <- failures])
        ]

-- | One engine unit per unit group: its reference unit, which every amount is converted to.
referenceUnits :: P.Package -> UnitDB
referenceUnits pkg =
    M.fromList
        [ (P.ugReference g, Unit (P.ugReference g) (P.ueName entry) (P.ueName entry) "")
        | g <- M.elems (P.pkUnitGroups pkg)
        , Just entry <- [M.lookup (P.ugReference g) (P.ugUnits g)]
        ]

-- | The reference unit of a flow's reference property.
flowUnit :: P.Package -> P.Flow -> Maybe UUID
flowUnit pkg flow = do
    groupId <- M.lookup (P.flReference flow) (P.pkPropertyGroups pkg)
    P.ugReference <$> M.lookup groupId (P.pkUnitGroups pkg)

data FlowTables = FlowTables
    { ftTech :: !TechFlowDB
    , ftBio :: !BioFlowDB
    , ftWaste :: !WasteFlowDB
    , ftNotices :: ![Notice]
    }

{- | The engine's three flow tables. A waste flow some process treats is also
a product flow, as an EcoSpold 2 treatment's reference is: the treatment's
column is keyed on it.
-}
flowTables :: P.Package -> M.Map UUID UUID -> FlowTables
flowTables pkg flowUnits =
    FlowTables
        { ftTech = M.fromList [(P.flId f, TechnosphereFlow (P.flId f) (P.flName f) u M.empty (P.flCas f) Nothing) | (f, u) <- withUnits, isTech f]
        , ftBio = M.fromList [(P.flId f, BiosphereFlow (P.flId f) (P.flName f) u M.empty (P.flCas f) Nothing (compartmentOf (P.flCategory f))) | (f, u) <- elementary]
        , ftWaste = M.fromList [(P.flId f, WasteFlow (P.flId f) (P.flName f) u M.empty (P.flCas f) Nothing) | (f, u) <- withUnits, P.flType f == P.WasteFlow]
        , ftNotices = [Unplaced (P.flName f) | (f, _) <- elementary, Nothing <- [compartmentOf (P.flCategory f)]]
        }
  where
    withUnits :: [(P.Flow, UUID)]
    withUnits = [(f, u) | f <- M.elems (P.pkFlows pkg), Just u <- [M.lookup (P.flId f) flowUnits]]

    elementary :: [(P.Flow, UUID)]
    elementary = filter ((== P.ElementaryFlow) . P.flType . fst) withUnits

    treated :: S.Set UUID
    treated = S.fromList [P.rxFlow x | p <- P.pkProcesses pkg, x <- P.prExchanges p, P.rxSide x == P.Consumed]

    isTech :: P.Flow -> Bool
    isTech f = case P.flType f of
        P.ProductFlow -> True
        P.WasteFlow -> S.member (P.flId f) treated
        P.ElementaryFlow -> False

{- | The compartment a category path names. Two spellings exist: one level
(@Elementary flows/Emission to air/…@) and the two levels of the Federal
Elementary Flow List (@Elementary flows/emission/air@). Ground under emission
is the soil; every resource is a natural resource, its medium kept as the
sub-compartment.
-}
compartmentOf :: Text -> Maybe Compartment
compartmentOf category = case T.splitOn "/" category of
    top : kind : rest | T.toLower top == "elementary flows" -> placed (T.toLower kind) rest
    _ -> Nothing
  where
    placed :: Text -> [Text] -> Maybe Compartment
    placed kind rest = case (kind, rest) of
        ("emission", medium : sub) -> (\m -> Compartment m (listToMaybe sub)) <$> emittedTo (T.toLower medium)
        ("resource", medium : _) -> Just (Compartment NaturalResource (Just medium))
        ("resource", []) -> Just (Compartment NaturalResource Nothing)
        ("emission to air", sub) -> Just (Compartment Air (listToMaybe sub))
        ("emission to water", sub) -> Just (Compartment Water (listToMaybe sub))
        ("emission to soil", sub) -> Just (Compartment Soil (listToMaybe sub))
        _ -> Nothing

    emittedTo :: Text -> Maybe Medium
    emittedTo medium = case medium of
        "air" -> Just Air
        "water" -> Just Water
        "ground" -> Just Soil
        "soil" -> Just Soil
        _ -> Nothing

-- | Whether a line offers its flow to others: a product made, or a waste taken in.
offers :: P.FlowType -> P.Side -> Bool
offers flowType side = case (flowType, side) of
    (P.ProductFlow, P.Produced) -> True
    (P.WasteFlow, P.Consumed) -> True
    (P.ProductFlow, P.Consumed) -> False
    (P.ProductFlow, P.Avoided) -> False
    (P.WasteFlow, P.Produced) -> False
    (P.WasteFlow, P.Avoided) -> False
    (P.ElementaryFlow, P.Produced) -> False
    (P.ElementaryFlow, P.Consumed) -> False
    (P.ElementaryFlow, P.Avoided) -> False

producerIndex :: P.Package -> M.Map UUID (M.Map UUID P.ProcessType)
producerIndex pkg =
    M.fromListWith
        M.union
        [ (P.rxFlow x, M.singleton (P.prId p) (P.prType p))
        | p <- P.pkProcesses pkg
        , x <- P.prExchanges p
        , Just flow <- [M.lookup (P.rxFlow x) (P.pkFlows pkg)]
        , offers (P.flType flow) (P.rxSide x)
        ]

-- | Which producer an input with no default provider is linked to.
data Choice = Sole !UUID | Tie !UUID

{- | openLCA's order: the only producer; else the only aggregated one; else
the first aggregated one, else the first, by identifier, which is stable
where openLCA's order is not, and reported.
-}
choose :: M.Map UUID P.ProcessType -> Maybe Choice
choose producers = case M.keys producers of
    [] -> Nothing
    [only] -> Just (Sole only)
    earliest : _ -> Just $ case M.keys (M.filter (== P.LciResult) producers) of
        [aggregated] -> Sole aggregated
        aggregated : _ -> Tie aggregated
        [] -> Tie earliest

data ReadProcess = ReadProcess
    { rpActivities :: ![((UUID, UUID), Activity)]
    , rpNotices :: ![Notice]
    }

-- | One line of a process, its amount computed and converted.
data Line = Line
    { lnAt :: !Int
    , lnRaw :: !P.RawExchange
    , lnFlow :: !P.Flow
    , lnUnit :: !UUID
    , lnAmount :: !Double
    , lnOutcome :: !Outcome
    }

data Outcome = NoFormula | Agrees | Diverges !Double | Refused !Text

readProcess :: Context -> P.Process -> Either [Unreadable] ReadProcess
readProcess cx p = case partitionEithers (zipWith (readLine cx p env) [0 ..] (P.prExchanges p)) of
    ([], lines') -> Right (assemble cx p env (envNotices <> formulaNotices p lines') lines')
    (failures, _) -> Left failures
  where
    env :: M.Map Text Double
    envNotices :: [Notice]
    (env, envNotices) = environment (P.pkGlobals (cxPackage cx)) p

{- | The parameters a process's formulas see: the globals, its own input
parameters over them (a process parameter shadows a global one of the same
name, case aside, as in openLCA), then its calculated ones, settled. Names
are lowercased, as the openLCA dialect reads every formula.
-}
environment :: [P.Parameter] -> P.Process -> (M.Map Text Double, [Notice])
environment globals p = (settled, [Unsettled (P.prName p <> " · " <> name) | (name, _) <- calculated, M.notMember name settled])
  where
    params :: [P.Parameter]
    params = M.elems (M.union (byName (P.prParameters p)) (byName globals))

    byName :: [P.Parameter] -> M.Map Text P.Parameter
    byName qs = M.fromList [(T.toLower (P.paName q), q) | q <- qs]

    given :: M.Map Text Double
    given = M.fromList [(T.toLower (P.paName q), v) | q <- params, P.InputValue v <- [P.paValue q]]

    calculated :: [(Text, Text)]
    calculated = [(T.toLower (P.paName q), f) | q <- params, P.Calculated f _ <- [P.paValue q]]

    settled :: M.Map Text Double
    settled = Expr.settle Expr.OpenLca given calculated

readLine :: Context -> P.Process -> M.Map Text Double -> Int -> P.RawExchange -> Either Unreadable Line
readLine cx p env at raw = do
    flow <- maybe (Left (MissingFlow (P.prName p <> " · " <> UUID.toText (P.rxFlow raw)))) Right (M.lookup (P.rxFlow raw) (P.pkFlows pkg))
    let named = P.prName p <> " · " <> P.flName flow
        (amount, outcome) = computed env (P.rxAmount raw) (P.rxFormula raw)
    unit <- maybe (Left (NoReferenceUnit named)) Right (M.lookup (P.flId flow) (cxFlowUnits cx))
    converted <- maybe (Left (NoConversion named)) Right (toReference pkg flow raw amount)
    pure Line{lnAt = at, lnRaw = raw, lnFlow = flow, lnUnit = unit, lnAmount = converted, lnOutcome = outcome}
  where
    pkg :: P.Package
    pkg = cxPackage cx

{- | A value the file stores, computed from its formula where it has one that
evaluates, as openLCA recomputes it: a line's amount, an allocation factor.
-}
computed :: M.Map Text Double -> Double -> Maybe Text -> (Double, Outcome)
computed env stored formula' = case formula' of
    Nothing -> (stored, NoFormula)
    Just formula -> case Expr.evaluate Expr.OpenLca env formula of
        Left refusal -> (stored, Refused (formula <> ": " <> Expr.describeRefusal refusal))
        Right value
            | abs (value - stored) <= 1e-9 * max 1 (max (abs value) (abs stored)) -> (value, Agrees)
            | otherwise -> (value, Diverges stored)

{- | An amount in the flow's reference unit: through the unit's factor in its
group, then, for a line stated in another property than the flow's own (a
gas in megajoules), through the flow's factor for that property.
-}
toReference :: P.Package -> P.Flow -> P.RawExchange -> Double -> Maybe Double
toReference pkg flow raw amount = do
    let property = fromMaybe (P.flReference flow) (P.rxProperty raw)
    groupId <- M.lookup property (P.pkPropertyGroups pkg)
    entry <- M.lookup (P.rxUnit raw) . P.ugUnits =<< M.lookup groupId (P.pkUnitGroups pkg)
    factor <- M.lookup property (P.flFactors flow)
    guard (factor /= 0)
    pure (amount * P.ueFactor entry / factor)

formulaNotices :: P.Process -> [Line] -> [Notice]
formulaNotices p = concatMap notice
  where
    notice :: Line -> [Notice]
    notice ln = case lnOutcome ln of
        NoFormula -> []
        Agrees -> []
        -- The computed amount is shown converted, the stored one as the file states it.
        Diverges stored -> [Divergent (named ln <> ": computes " <> tshow (lnAmount ln) <> " in the flow's reference unit, the file stores " <> tshow stored)]
        Refused why -> [Unevaluable (named ln <> ": " <> why)]

    named :: Line -> Text
    named ln = P.prName p <> " · " <> P.flName (lnFlow ln)

    tshow :: Double -> Text
    tshow = T.pack . show

-- | The shares a process's products carry, and the pairs causal allocation adds.
data Split = Split
    { spShares :: !(M.Map Int Double)
    , spPairs :: ![((Int, Int), Double)]
    , spNotices :: ![Notice]
    }

{- | How a process with several products is divided, by its own method. A
product with no factor for that method carries the whole inventory, as in
openLCA, and so does every product of a process naming no method; both are
said. Causal factors are per (product, line); a line with none stays whole. A
factor's formula is used over its stored value, as openLCA does; a formula
that disagrees with the file, or has no value (the stored one then kept), is
said too.
-}
split :: M.Map Text Double -> P.Process -> [Line] -> [Line] -> Split
split env p lines' products
    | length products < 2 = Split M.empty [] []
    | otherwise = case P.prAllocation p of
        Just P.Physical -> perProduct P.Physical
        Just P.Economic -> perProduct P.Economic
        Just P.Causal -> causal
        Just P.NoAllocation -> whole
        Nothing -> whole
  where
    whole :: Split
    whole = Split (M.fromList [(lnAt ln, 100) | ln <- products]) [] [WithoutFactor (P.prName p)]

    perProduct :: P.AllocationMethod -> Split
    perProduct method =
        Split
            { spShares = M.fromList [(lnAt ln, maybe 100 (* 100) (factorFor method ln)) | ln <- products]
            , spPairs = []
            , spNotices = [WithoutFactor (P.prName p <> " · " <> P.flName (lnFlow ln)) | ln <- products, Nothing <- [factorFor method ln]] <> factorNotices method
            }

    factorFor :: P.AllocationMethod -> Line -> Maybe Double
    factorFor method ln =
        listToMaybe
            [ value f
            | f <- P.prFactors p
            , P.afMethod f == method
            , P.afProduct f == P.flId (lnFlow ln)
            , Nothing <- [P.afExchange f]
            ]

    causal :: Split
    causal = case partitionEithers ([pair f | f <- P.prFactors p, P.afMethod f == P.Causal]) of
        (strays, []) -> whole{spNotices = WithoutFactor (P.prName p) : strays <> factorNotices P.Causal}
        (strays, pairs) -> Split (M.fromList [(lnAt ln, 100) | ln <- products]) pairs (strays <> factorNotices P.Causal)

    pair :: P.AllocationFactor -> Either Notice ((Int, Int), Double)
    pair f = maybe (Left (StrayFactor (P.prName p))) Right $ do
        productAt <- listToMaybe [lnAt ln | ln <- products, P.flId (lnFlow ln) == P.afProduct f]
        internalId <- P.afExchange f
        lineAt <- listToMaybe [lnAt ln | ln <- lines', P.rxInternalId (lnRaw ln) == internalId]
        pure ((productAt, lineAt), value f * 100)

    value :: P.AllocationFactor -> Double
    value = fst . factorOutcome

    factorOutcome :: P.AllocationFactor -> (Double, Outcome)
    factorOutcome f = computed env (P.afValue f) (P.afFormula f)

    -- The factors of the method in use whose formula disagrees with the file or has no value.
    factorNotices :: P.AllocationMethod -> [Notice]
    factorNotices method = concatMap factorNotice [f | f <- P.prFactors p, P.afMethod f == method]

    factorNotice :: P.AllocationFactor -> [Notice]
    factorNotice f = case factorOutcome f of
        (_, NoFormula) -> []
        (_, Agrees) -> []
        (value', Diverges stored) -> [Divergent (named <> foldMap (" " <>) (P.afFormula f) <> ": computes " <> tshow value' <> ", the file stores " <> tshow stored)]
        (_, Refused why) -> [Unevaluable (named <> " " <> why)]
      where
        named :: Text
        named = P.prName p <> " · " <> productName f <> " · allocation factor"

    tshow :: Double -> Text
    tshow = T.pack . show

    productName :: P.AllocationFactor -> Text
    productName f = maybe (UUID.toText (P.afProduct f)) (P.flName . lnFlow) (find ((== P.afProduct f) . P.flId . lnFlow) products)

assemble :: Context -> P.Process -> M.Map Text Double -> [Notice] -> [Line] -> ReadProcess
assemble cx p env notices lines' =
    ReadProcess
        { rpActivities =
            [ ((P.prId p, referenceFlow a), a)
            | a <- NE.toList (allocateWith (cxAllocating cx) (exchangeShares (spPairs division)) activity)
            ]
        , rpNotices = notices <> spNotices division <> concatMap snd built
        }
  where
    products :: [Line]
    products = filter (\ln -> offers (P.flType (lnFlow ln)) (P.rxSide (lnRaw ln))) lines'

    division :: Split
    division = split env p lines' products

    built :: [(Exchange, [Notice])]
    built = map (engineExchange cx p (spShares division)) lines'

    location :: Text
    location = maybe "" (\l -> M.findWithDefault "" l (P.pkLocations (cxPackage cx))) (P.prLocation p)

    activity :: Activity
    activity =
        Activity
            { activityName = P.prName p
            , activityDescription = maybeToList (P.prDescription p)
            , activityDocumentation = []
            , activitySynonyms = M.empty
            , activityClassification = if T.null (P.prCategory p) then M.empty else M.singleton "Category" (P.prCategory p)
            , activityLocation = location
            , activityLocationSource = declaredLocationSource location
            , activityUnit = fromMaybe "" $ do
                ref <- find (P.rxReference . lnRaw) lines'
                unitName <$> M.lookup (lnUnit ref) (alUnitDB (cxAllocating cx))
            , exchanges = map fst built
            , activityParams = env
            , activityParamExprs = M.fromList [(P.paName q, f) | q <- P.prParameters p, P.Calculated f _ <- [P.paValue q]]
            , activityNativeType = Nothing
            , activityNativeId = Nothing
            , activityFormulaCheck = formulaCheck lines'
            , activityDates = noDates
            }

    -- A process the gate will refuse for having no reference keeps its own id as its product.
    referenceFlow :: Activity -> UUID
    referenceFlow a = fromMaybe (P.prId p) (listToMaybe [exchangeFlowId ex | ex <- exchanges a, exchangeIsReference ex])

formulaCheck :: [Line] -> Maybe FormulaCheck
formulaCheck lines'
    | null outcomes = Nothing
    | otherwise =
        Just
            FormulaCheck
                { fcEvaluated = length [() | (_, Agrees) <- outcomes] + length divergent
                , fcDivergent = length divergent
                , fcUnevaluable = length refused
                , fcDivergentExample = listToMaybe divergent
                , fcUnevaluableExample = listToMaybe refused
                }
  where
    outcomes :: [(Text, Outcome)]
    outcomes = [(P.flName (lnFlow ln), lnOutcome ln) | ln <- lines', hasFormula (lnOutcome ln)]

    divergent, refused :: [Text]
    divergent = [name <> ": the file stores " <> T.pack (show stored) | (name, Diverges stored) <- outcomes]
    refused = [name <> ": " <> why | (name, Refused why) <- outcomes]

    hasFormula :: Outcome -> Bool
    hasFormula outcome = case outcome of
        NoFormula -> False
        Agrees -> True
        Diverges _ -> True
        Refused _ -> True

-- | The engine exchange a line becomes, and what linking it noticed.
engineExchange :: Context -> P.Process -> M.Map Int Double -> Line -> (Exchange, [Notice])
engineExchange cx p shares ln = case (P.flType flow, P.rxSide raw) of
    (P.ElementaryFlow, P.Produced) -> (biosphere Emission, [])
    (P.ElementaryFlow, P.Consumed) -> (biosphere Resource, [])
    -- openLCA offers "avoided" on product and waste lines; an elementary one is read as the input it is stored as.
    (P.ElementaryFlow, P.Avoided) -> (biosphere Resource, [])
    (P.ProductFlow, P.Produced) -> (made amount, [])
    (P.ProductFlow, P.Consumed) -> linked Input
    (P.ProductFlow, P.Avoided) -> linked AvoidedProduct
    (P.WasteFlow, P.Consumed) -> (made (negate amount), [])
    (P.WasteFlow, P.Produced) -> waste amount
    (P.WasteFlow, P.Avoided) -> waste (negate amount)
  where
    raw :: P.RawExchange
    raw = lnRaw ln

    flow :: P.Flow
    flow = lnFlow ln

    amount :: Double
    amount = lnAmount ln

    location :: ExchangeLocation
    location = readExchangeLocation (maybe "" (\l -> M.findWithDefault "" l (P.pkLocations (cxPackage cx))) (P.rxLocation raw))

    biosphere :: BioDirection -> Exchange
    biosphere direction =
        BiosphereExchange
            { bioFlowId = P.flId flow
            , bioAmount = amount
            , bioUnitId = lnUnit ln
            , bioDirection = direction
            , bioLocation = location
            , bioComment = P.rxDescription raw
            , bioPedigree = Nothing
            }

    technosphere :: TechRole -> Double -> Supplier -> Maybe DeclaredShare -> Exchange
    technosphere role a link share =
        TechnosphereExchange
            { techFlowId = P.flId flow
            , techAmount = a
            , techUnitId = lnUnit ln
            , techRole = role
            , techActivityLinkId = suTarget link
            , techSupplierClaim = suClaim link
            , techLocation = location
            , techComment = P.rxDescription raw
            , techPedigree = Nothing
            , techShare = share
            , techClassification = M.empty
            , techProperties = noProperties
            }

    made :: Double -> Exchange
    made a =
        technosphere
            (if P.rxReference raw then ReferenceProduct else Coproduct)
            a
            (Supplier Nothing ClaimByProduct [])
            ((\pct -> DeclaredShare{dsPercent = pct, dsFormula = Nothing}) <$> M.lookup (lnAt ln) shares)

    linked :: TechRole -> (Exchange, [Notice])
    linked role =
        ( technosphere role amount supplier Nothing
        , suNotices supplier
        )

    waste :: Double -> (Exchange, [Notice])
    waste a =
        ( WasteExchange
            { waFlowId = P.flId flow
            , waAmount = a
            , waUnitId = lnUnit ln
            , waIsInput = False
            , waActivityLinkId = suTarget supplier
            , waSupplierClaim = suClaim supplier
            , waLocation = location
            , waComment = P.rxDescription raw
            , waPedigree = Nothing
            }
        , suNotices supplier
        )

    named :: Text
    named = P.prName p <> " · " <> P.flName flow

    -- The default provider when there is one, else openLCA's choice among the producers.
    supplier :: Supplier
    supplier = case P.rxProvider raw of
        Just provider -> Supplier (Just provider) (ClaimById provider) []
        Nothing -> case choose (M.findWithDefault M.empty (P.flId flow) (cxProducers cx)) of
            Nothing -> Supplier Nothing ClaimByProduct [CutOff named]
            Just (Sole pid) -> Supplier (Just pid) ClaimByProduct []
            Just (Tie pid) -> Supplier (Just pid) ClaimByProduct [Tied named]

-- | Where a line is linked, how the source named its supplier, and what choosing it noticed.
data Supplier = Supplier
    { suTarget :: !(Maybe UUID)
    , suClaim :: !SupplierClaim
    , suNotices :: ![Notice]
    }

{- | What the load says, one paragraph per kind of notice: how many, and the
first ten named.
-}
describeNotices :: [Notice] -> [Text]
describeNotices notices =
    [ T.pack (show (length details)) <> " " <> title <> foldMap ("\n  " <>) (take 10 (concat details))
    | (title, details) <- M.toList (M.fromListWith (flip (<>)) [(t, [maybeToList d]) | (t, d) <- map headline notices])
    ]
  where
    headline :: Notice -> (Text, Maybe Text)
    headline n = case n of
        Divergent what -> ("formulas compute another amount than the file stores; the computed one is kept:", Just what)
        Unevaluable what -> ("formulas could not be evaluated; the stored amount is kept:", Just what)
        Unsettled what -> ("calculated parameters have no value:", Just what)
        WithoutFactor what -> ("products carry their process's whole inventory, for want of an allocation factor or method:", Just what)
        StrayFactor what -> ("processes have causal factors naming a product or line they do not have:", Just what)
        Tied what -> ("inputs had several producers; the first by identifier was linked:", Just what)
        CutOff what -> ("inputs have no producer in the package and stay cut off:", Just what)
        Unplaced what -> ("elementary flows have no compartment the engine knows:", Just what)
        NotRead P.Uncertainty -> ("uncertainty distributions are not read yet", Nothing)
        NotRead P.DataQuality -> ("data quality entries are not read yet", Nothing)
        NotRead P.SocialAspects -> ("processes with social aspects: those are not read yet", Nothing)
        NotRead P.Costs -> ("lines with a cost: costs are not read yet", Nothing)
