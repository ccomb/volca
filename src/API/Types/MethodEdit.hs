{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types.MethodEdit where

import API.JsonOptions (Stripped (..), parseClosed)
import API.Types.MethodComparison
import Config (MethodPatch (..), MethodPatchMatch (..))
import Control.Lens ((&), (?~))
import Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.OpenApi (NamedSchema (..), OpenApiType (..), ToSchema (..), enum_, type_)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Method.EditPlan (CategoryDraft (..), CategoryEdit (..), FactorEdit (..), FactorTarget (..))
import Method.Journal (Exact (..))
import Method.ScoringEdit (RowDraft (..), ScoringEdit (..))
import Method.Types (FlowDirection, MethodCF (..))
import qualified Method.Types as MT
import Types (
    PatchOp (..),
    UUID,
 )

-- ---------------------------------------------------------------------------
-- Changing a method collection of one's own
-- ---------------------------------------------------------------------------

-- | What one request does. Read from the five words below; any other is refused naming them.
data FactorEditOp = SetOne | RemoveOne | AddOne | ScaleMany | SetMany
    deriving (Eq, Show, Enum, Bounded)

factorEditOpName :: FactorEditOp -> Text
factorEditOpName = \case
    SetOne -> "set"
    RemoveOne -> "remove"
    AddOne -> "add"
    ScaleMany -> "scale"
    SetMany -> "set-all"

instance FromJSON FactorEditOp where
    parseJSON = withText "op" $ \t ->
        maybe
            (fail ("op " <> show t <> " is none of " <> T.unpack (T.intercalate ", " (map factorEditOpName [minBound .. maxBound]))))
            pure
            (lookup t [(factorEditOpName o, o) | o <- [minBound .. maxBound]])

instance ToJSON FactorEditOp where
    toJSON = toJSON . factorEditOpName

instance ToSchema FactorEditOp where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "FactorEditOp") $
                mempty
                    & type_
                        ?~ OpenApiString
                    & enum_
                        ?~ map (toJSON . factorEditOpName) [minBound .. maxBound]

{- | One change asked of a collection's factors. Which fields count follows
from 'op': a set names the factor (its category, flow and place, and its
present value when several answer there) and the new value; a removal names
the factor; an addition names the category and the whole factor; a scale or a
set-all names a selector and the factor or the value it applies.
-}
data FactorEditRequest = FactorEditRequest
    { ferOp :: FactorEditOp
    , ferMethodId :: Maybe UUID
    , ferFlowId :: Maybe UUID
    , ferLocation :: Maybe Text
    , ferValue :: Maybe Double
    , ferNewValue :: Maybe Double
    , ferScale :: Maybe Double
    , ferMatch :: Maybe FactorMatchAPI
    , ferFactor :: Maybe NewFactorAPI
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FactorEditRequest)

instance FromJSON FactorEditRequest where
    parseJSON = parseClosed

-- | The factors a selector reaches, as the configuration's @[[methods.patches]]@ write them.
data FactorMatchAPI = FactorMatchAPI
    { fmaCategory :: Maybe Text
    , fmaFlowName :: Maybe Text
    , fmaFlowNamePrefix :: Maybe Text
    , fmaCas :: Maybe Text
    , fmaSubcompartmentContains :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped FactorMatchAPI)

instance FromJSON FactorMatchAPI where
    parseJSON = parseClosed

-- | A factor written whole, as a collection holds it.
data NewFactorAPI = NewFactorAPI
    { nfaFlowId :: UUID
    , nfaName :: Text
    , nfaDirection :: FlowDirection
    , nfaValue :: Double
    , nfaCompartment :: Maybe CompartmentAPI
    , nfaCas :: Maybe Text
    , nfaUnit :: Text
    , nfaLocation :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped NewFactorAPI)

instance FromJSON NewFactorAPI where
    parseJSON = parseClosed

data CompartmentAPI = CompartmentAPI
    { cpaMedium :: Text
    , cpaSubcompartment :: Text
    , cpaQualifier :: Text
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped CompartmentAPI)

instance FromJSON CompartmentAPI where
    parseJSON = parseClosed

-- | What one request does to a collection's categories. Read from the four words below; any other is refused naming them.
data CategoryEditOp = AddCategoryOp | RenameCategoryOp | SetCategoryUnitOp | RemoveCategoryOp
    deriving (Eq, Show, Enum, Bounded)

categoryEditOpName :: CategoryEditOp -> Text
categoryEditOpName = \case
    AddCategoryOp -> "add"
    RenameCategoryOp -> "rename"
    SetCategoryUnitOp -> "set-unit"
    RemoveCategoryOp -> "remove"

instance FromJSON CategoryEditOp where
    parseJSON = withText "op" $ \t ->
        maybe
            (fail ("op " <> show t <> " is none of " <> T.unpack (T.intercalate ", " (map categoryEditOpName [minBound .. maxBound]))))
            pure
            (lookup t [(categoryEditOpName o, o) | o <- [minBound .. maxBound]])

instance ToJSON CategoryEditOp where
    toJSON = toJSON . categoryEditOpName

instance ToSchema CategoryEditOp where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "CategoryEditOp") $
                mempty
                    & type_
                        ?~ OpenApiString
                    & enum_
                        ?~ map (toJSON . categoryEditOpName) [minBound .. maxBound]

{- | One change asked of a collection's categories. An addition names the
name and the unit (the impact category and methodology are optional); a
rename, a change of unit and a removal name the category by 'methodId'.
-}
data CategoryEditRequest = CategoryEditRequest
    { cerOp :: CategoryEditOp
    , cerMethodId :: Maybe UUID
    , cerName :: Maybe Text
    , cerUnit :: Maybe Text
    , cerImpactCategory :: Maybe Text
    , cerMethodology :: Maybe Text
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped CategoryEditRequest)

instance FromJSON CategoryEditRequest where
    parseJSON = parseClosed

-- | What a change asks of a collection's categories, or the field it lacks.
toCategoryEdit :: CategoryEditRequest -> Either Text CategoryEdit
toCategoryEdit req = case cerOp req of
    AddCategoryOp ->
        maybe (Left "an add names name and unit") Right $
            (\name unit -> NewCategory (CategoryDraft name unit (cerImpactCategory req) (cerMethodology req))) <$> cerName req <*> cerUnit req
    RenameCategoryOp -> maybe (Left "a rename names methodId and name") Right (Rename <$> cerMethodId req <*> cerName req)
    SetCategoryUnitOp -> maybe (Left "a set-unit names methodId and unit") Right (ChangeUnit <$> cerMethodId req <*> cerUnit req)
    RemoveCategoryOp -> maybe (Left "a remove names methodId") (Right . Delete) (cerMethodId req)

{- | What a change did: the journal line recording it, how many factors it
touched, the one factor's value before and after, and the one category its
line names (for an addition, the category it made).
-}
data MethodEditResponse = MethodEditResponse
    { merLine :: Int
    , merTouched :: Int
    , merBefore :: Maybe Double
    , merAfter :: Maybe Double
    , merMethodId :: Maybe UUID
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped MethodEditResponse)

-- | Why a journal line is there: a change asked for, the undo of another line, or what a copy took from its source's configuration.
data HistoryKindAPI = ChangeLine | UndoLine | ConfigurationLine
    deriving (Eq, Show, Enum, Bounded)

historyKindName :: HistoryKindAPI -> Text
historyKindName = \case
    ChangeLine -> "change"
    UndoLine -> "undo"
    ConfigurationLine -> "configuration"

instance ToJSON HistoryKindAPI where
    toJSON = toJSON . historyKindName

instance ToSchema HistoryKindAPI where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "HistoryKindAPI") $
                mempty
                    & type_
                        ?~ OpenApiString
                    & enum_
                        ?~ map (toJSON . historyKindName) [minBound .. maxBound]

-- | One line of a collection's journal.
data MethodHistoryEntry = MethodHistoryEntry
    { mheLine :: Int
    , mheAt :: Text
    , mheBy :: Maybe Text
    -- ^ the public name of the account that made it; absent when the engine was not told
    , mheKind :: HistoryKindAPI
    , mheUndoes :: Maybe Int
    -- ^ the line this one undoes, for an undo
    , mheInEffect :: Bool
    -- ^ false once a later line in effect undoes it
    , mheChange :: MethodChangeAPI
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped MethodHistoryEntry)

-- | What a journal line changed, in the words a reader of the collection uses.
data MethodChangeAPI
    = FactorSet {fstCategory :: Text, fstFactor :: FactorSide, fstBefore :: Double, fstAfter :: Double}
    | FactorRemoved {frmCategory :: Text, frmFactor :: FactorSide}
    | FactorAdded {fadCategory :: Text, fadFactor :: FactorSide}
    | FactorsPatched {fptSelector :: Text, fptTouched :: Int}
    | FactorsRestored {frsTouched :: Int}
    | UnregionalizedSet {unrBefore :: [Text], unrAfter :: [Text]}
    | ScoringSetCreated {sscrName :: Text}
    | ScoringSetRemoved {ssrmName :: Text}
    | CategoryAdded {cadName :: Text, cadFactors :: Int}
    | CategoryRenamed {crnBefore :: Text, crnAfter :: Text}
    | CategoryUnitSet {cusCategory :: Text, cusBefore :: Text, cusAfter :: Text}
    | CategoryRemoved {crmName :: Text, crmFactors :: Int}
    | ScoringSetChanged {sschSet :: Text, sschGesture :: ScoringGestureAPI}
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped MethodChangeAPI)

-- | What one change did to a scoring set, as the person who asked for it said it.
data ScoringGestureAPI
    = RowAdded {rwaLabel :: Text}
    | RowChanged {rwcLabel :: Text}
    | RowRemoved {rwrLabel :: Text}
    | SetRenamed {strBefore :: Text, strAfter :: Text}
    | SetUnitChanged {sucBefore :: Text, sucAfter :: Text}
    | MultiplierSet {mlsBefore :: Maybe Double, mlsAfter :: Maybe Double}
    | FormulaWritten {fmwVariable :: Text}
    | ScoreAdded {scaName :: Text}
    | ScoreChanged {scgName :: Text}
    | ScoreRemoved {scrName :: Text}
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ScoringGestureAPI)

-- | What one request does to a collection's scoring sets. Read from the words below; any other is refused naming them.
data ScoringEditOp
    = CreateSetOp
    | RemoveSetOp
    | RenameSetOp
    | SetSetUnitOp
    | SetMultiplierOp
    | AddRowOp
    | ChangeRowOp
    | RemoveRowOp
    | SetFormulaOp
    | SetScoreOp
    | RemoveScoreOp
    deriving (Eq, Show, Enum, Bounded)

scoringEditOpName :: ScoringEditOp -> Text
scoringEditOpName = \case
    CreateSetOp -> "create"
    RemoveSetOp -> "remove"
    RenameSetOp -> "rename"
    SetSetUnitOp -> "set-unit"
    SetMultiplierOp -> "set-multiplier"
    AddRowOp -> "add-row"
    ChangeRowOp -> "change-row"
    RemoveRowOp -> "remove-row"
    SetFormulaOp -> "set-formula"
    SetScoreOp -> "set-score"
    RemoveScoreOp -> "remove-score"

instance FromJSON ScoringEditOp where
    parseJSON = withText "op" $ \t ->
        maybe
            (fail ("op " <> show t <> " is none of " <> T.unpack (T.intercalate ", " (map scoringEditOpName [minBound .. maxBound]))))
            pure
            (lookup t [(scoringEditOpName o, o) | o <- [minBound .. maxBound]])

instance ToJSON ScoringEditOp where
    toJSON = toJSON . scoringEditOpName

instance ToSchema ScoringEditOp where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "ScoringEditOp") $
                mempty
                    & type_
                        ?~ OpenApiString
                    & enum_
                        ?~ map (toJSON . scoringEditOpName) [minBound .. maxBound]

{- | One change asked of a collection's scoring sets, each naming its set.
A creation names the rows (and optionally the unit); a rename names the new
'name'; a change of unit the 'unit'; a change of multiplier the 'multiplier',
or none to drop it; a row is added as a 'row', changed or removed by its
'variable'; a formula is written for an existing computed 'variable'; a score
is set with its 'formula' or removed, by its 'score'.
-}
data ScoringEditRequest = ScoringEditRequest
    { serOp :: ScoringEditOp
    , serSet :: Text
    , serName :: Maybe Text
    , serUnit :: Maybe Text
    , serMultiplier :: Maybe Double
    , serVariable :: Maybe Text
    , serFormula :: Maybe Text
    , serScore :: Maybe Text
    , serRow :: Maybe RowDraftAPI
    , serRows :: Maybe [RowDraftAPI]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ScoringEditRequest)

instance FromJSON ScoringEditRequest where
    parseJSON = parseClosed

-- | A row as a caller writes it: the categories it groups by their identifier, each times its coefficient.
data RowDraftAPI = RowDraftAPI
    { rdaLabel :: Text
    , rdaUnit :: Maybe Text
    , rdaTerms :: [DraftTermAPI]
    , rdaNormalization :: Maybe Exact
    -- ^ a number, or "Infinity" for a row counted as zero, as a set read from a file can hold
    , rdaWeight :: Maybe Double
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped RowDraftAPI)

instance FromJSON RowDraftAPI where
    parseJSON = parseClosed

data DraftTermAPI = DraftTermAPI
    { dtaMethodId :: UUID
    , dtaCoefficient :: Double
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped DraftTermAPI)

instance FromJSON DraftTermAPI where
    parseJSON = parseClosed

-- | What a change asks of a collection's scoring sets, or the field it lacks.
toScoringEdit :: ScoringEditRequest -> Either Text ScoringEdit
toScoringEdit req = case serOp req of
    CreateSetOp -> NewSet set (serUnit req) <$> maybe (Left "a create names rows") (mapM toRowDraft) (serRows req)
    RemoveSetOp -> Right (DeleteSet set)
    RenameSetOp -> needs "a rename names name" (RenameScoringSet set <$> serName req)
    SetSetUnitOp -> needs "a set-unit names unit" (ChangeSetUnit set <$> serUnit req)
    SetMultiplierOp -> Right (ChangeMultiplier set (serMultiplier req))
    AddRowOp -> maybe (Left "an add-row names row") (fmap (AddRow set) . toRowDraft) (serRow req)
    ChangeRowOp -> maybe (Left "a change-row names variable and row") (\(v, r) -> ChangeRow set v <$> toRowDraft r) ((,) <$> serVariable req <*> serRow req)
    RemoveRowOp -> needs "a remove-row names variable" (DeleteRow set <$> serVariable req)
    SetFormulaOp -> needs "a set-formula names variable and formula" (WriteFormula set <$> serVariable req <*> serFormula req)
    SetScoreOp -> needs "a set-score names score and formula" (PutScore set <$> serScore req <*> serFormula req)
    RemoveScoreOp -> needs "a remove-score names score" (DeleteScore set <$> serScore req)
  where
    set :: Text
    set = serSet req
    needs :: Text -> Maybe ScoringEdit -> Either Text ScoringEdit
    needs missing = maybe (Left missing) Right
    toRowDraft :: RowDraftAPI -> Either Text RowDraft
    toRowDraft r =
        maybe
            (Left ("the row '" <> rdaLabel r <> "' groups no category"))
            (\terms -> Right (RowDraft (rdaLabel r) (rdaUnit r) terms (exact <$> rdaNormalization r) (rdaWeight r)))
            (NE.nonEmpty [(dtaMethodId t, dtaCoefficient t) | t <- rdaTerms r])

-- | A scoring set as rows of a guided grouping, with the scores reading them and every name a formula can read.
data ScoringSetAPI = ScoringSetAPI
    { ssaName :: Text
    , ssaUnit :: Text
    , ssaDisplayMultiplier :: Maybe Double
    , ssaRows :: [ScoringRowAPI]
    , ssaScores :: [ScoreAPI]
    , ssaVariables :: M.Map Text Text
    -- ^ every name a formula of the set can read → the category it reads, or the label of the row it computes
    , ssaWeighs :: Bool
    -- ^ Its scores rest on value choices (see 'Method.Scoring.weighs'): no ground for a comparison disclosed to the public
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ScoringSetAPI)

data ScoringRowAPI = ScoringRowAPI
    { sraVariable :: Text
    , sraLabel :: Text
    , sraUnit :: Maybe Text
    , sraTerms :: RowTermsAPI
    , sraNormalization :: Maybe Exact
    -- ^ "Infinity" when the set counts the row as zero, as a file writing a normalization of 0 is read
    , sraWeight :: Maybe Double
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ScoringRowAPI)

-- | What a row adds up: categories, each times its coefficient, or a formula that is no such sum.
data RowTermsAPI
    = RowGrouped {grpCategories :: [RowTermAPI]}
    | RowWritten {wrtFormula :: Text}
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped RowTermsAPI)

-- | A category a row reads; its identifier when the collection holds a category of that name.
data RowTermAPI = RowTermAPI
    { rtaCategory :: Text
    , rtaMethodId :: Maybe UUID
    , rtaCoefficient :: Double
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped RowTermAPI)

-- | A score, and whether it is the sum of the rows, which a new row joins.
data ScoreAPI = ScoreAPI
    { scoName :: Text
    , scoFormula :: Text
    , scoSumOfRows :: Bool
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ScoreAPI)

{- | A flow a collection characterizes, once per direction and compartment.
Ordered by name, then compartment: the order a list of them is read in.
-}
data MethodFlowAPI = MethodFlowAPI
    { mflName :: Text
    , mflCompartment :: Maybe CompartmentAPI
    , mflFlowId :: UUID
    , mflDirection :: FlowDirection
    , mflCas :: Maybe Text
    , mflUnit :: Text
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped MethodFlowAPI)

-- | What a request asks, or why it asks nothing this engine can do.
toFactorEdit :: FactorEditRequest -> Either Text FactorEdit
toFactorEdit req =
    mapM_ finite numbers >> case ferOp req of
        SetOne -> maybe (Left "a set names methodId, flowId and newValue") Right $ do
            target <- named
            SetValue target <$> ferNewValue req
        RemoveOne -> maybe (Left "a remove names methodId and flowId") (Right . Remove) named
        AddOne ->
            maybe (Left "an add names methodId and factor") Right $
                Add <$> ferMethodId req <*> (newFactor <$> ferFactor req)
        ScaleMany -> selector "a scale names match and scale" (ScaleBy <$> ferScale req)
        SetMany -> selector "a set-all names match and newValue" (SetValueTo <$> ferNewValue req)
  where
    -- JSON writes an infinite or undefined number as null, so the journal
    -- would read back another value than the one applied.
    numbers :: [(Text, Maybe Double)]
    numbers = [("value", ferValue req), ("newValue", ferNewValue req), ("scale", ferScale req), ("factor.value", nfaValue <$> ferFactor req)]
    finite :: (Text, Maybe Double) -> Either Text ()
    finite (field, v)
        | any (\x -> isNaN x || isInfinite x) v = Left (field <> " must be a finite number")
        | otherwise = Right ()
    named :: Maybe FactorTarget
    named = (\m f -> FactorTarget m f (ferLocation req) (ferValue req)) <$> ferMethodId req <*> ferFlowId req
    selector :: Text -> Maybe PatchOp -> Either Text FactorEdit
    selector missing op = case (ferMatch req, op) of
        (Just m, Just o)
            | emptyMatch m -> Left "match: at least one selector field must be set"
            | otherwise -> Right (Patch (MethodPatch Nothing (matchOf m) o))
        _ -> Left missing
    emptyMatch :: FactorMatchAPI -> Bool
    emptyMatch m = all isNothing [fmaCategory m, fmaFlowName m, fmaFlowNamePrefix m, fmaCas m, fmaSubcompartmentContains m]
    matchOf :: FactorMatchAPI -> MethodPatchMatch
    matchOf m = MethodPatchMatch (fmaCategory m) (fmaFlowName m) (fmaFlowNamePrefix m) (fmaCas m) (fmaSubcompartmentContains m)
    newFactor :: NewFactorAPI -> MethodCF
    newFactor n =
        MethodCF
            { mcfFlowRef = nfaFlowId n
            , mcfFlowName = nfaName n
            , mcfDirection = nfaDirection n
            , mcfValue = nfaValue n
            , mcfCompartment = (\c -> MT.Compartment (cpaMedium c) (cpaSubcompartment c) (cpaQualifier c)) <$> nfaCompartment n
            , mcfCAS = nfaCas n
            , mcfUnit = nfaUnit n
            , mcfConsumerLocation = nfaLocation n
            }
