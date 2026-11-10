{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types.Comparison where

import API.JsonOptions (Stripped (..), parseClosed)
import API.Types.Common
import Control.Lens ((&), (.~), (?~))
import Data.Aeson
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.JournalFile (Source)
import Data.OpenApi (NamedSchema (..), OpenApiType (..), Referenced (..), ToSchema (..), enum_, properties, required, type_)
import Data.Text (Text)
import GHC.Generics
import Types (
    BioDirection (..),
    Compartment (..),
    DatasetDates (..),
    TechRole,
    UUID,
 )

-- ---------------------------------------------------------------------------
-- Comparing two activities, or two versions of a database
-- ---------------------------------------------------------------------------

-- | The lines of one flow in one unit, as one side writes them, summed.
data Quantity = Quantity
    { qtyAmount :: !Double
    , qtyUnit :: !Text
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped Quantity)

-- | Which side of a waste line an activity stands on.
data WasteSide = WasteInput | WasteOutput
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

{- | What a line does in its activity. The kind of exchange is part of it, so
a flow that moves from input to coproduct is one line gone and another added,
never one line with a new amount.
-}
data LineRole
    = TechLine {tlRole :: !TechRole}
    | BioLine {blDirection :: !BioDirection}
    | WasteLine {wlSide :: !WasteSide}
    deriving (Eq, Ord, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped LineRole)

-- | How a line was found again in the other activity.
data LineMatch
    = -- | The same flow identifier, in the same role.
      SameFlow
    | -- | The same flow name, case and a trailing geography aside, in the same compartment and role.
      SameFlowName
    deriving (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data LineChange
    = LineAdded {laAfter :: !Quantity}
    | LineRemoved {lrBefore :: !Quantity}
    | LineChanged {lcMatch :: !LineMatch, lcBefore :: !Quantity, lcAfter :: !Quantity}
    | -- | The one line of this flow on each side comes from another supplier.
      SupplierChanged {scBefore :: !Supplier, scAfter :: !Supplier}
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped LineChange)

{- | One line that differs. The flow is named as the base side has it, or as
the other side has it when the line was added.
-}
data ExchangeChange = ExchangeChange
    { ecFlowId :: !UUID
    , ecFlowName :: !Text
    , ecCompartment :: !(Maybe Compartment)
    , ecRole :: !LineRole
    , ecChange :: !LineChange
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ExchangeChange)

data UncomparedReason
    = -- | One flow written in several units on a side: no sum of kg and g reads as either.
      MixedUnits {muBaseUnits :: ![Text], muOtherUnits :: ![Text]}
    | -- | Several distinct flows answer to one name on a side.
      SeveralFlows {sfBaseFlows :: ![UUID], sfOtherFlows :: ![UUID]}
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped UncomparedReason)

-- | A line the comparison could not judge, and why.
data UncomparedLine = UncomparedLine
    { ulFlowName :: !Text
    , ulCompartment :: !(Maybe Compartment)
    , ulRole :: !LineRole
    , ulReason :: !UncomparedReason
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped UncomparedLine)

{- | A field of the two activities that differs. The product's amount and unit
are not among them: the reference line already says so.
-}
data SummaryChange
    = ActivityNameChanged {ancBefore :: !Text, ancAfter :: !Text}
    | LocationChanged {locBefore :: !Text, locAfter :: !Text}
    | ProductNameChanged {pncBefore :: !Text, pncAfter :: !Text}
    | AllocationChanged {alcBefore :: !(Maybe Double), alcAfter :: !(Maybe Double)}
    | DatesChanged {dacBefore :: !DatasetDates, dacAfter :: !DatasetDates}
    | -- | One text per paragraph.
      DescriptionChanged {dscBefore :: ![Text], dscAfter :: ![Text]}
    deriving (Eq, Show, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped SummaryChange)

-- | Two activities side by side: nothing listed means they say the same thing.
data ActivityComparison = ActivityComparison
    { acmpBase :: !ActivitySummary
    , acmpOther :: !ActivitySummary
    , acmpSummary :: ![SummaryChange]
    , acmpExchanges :: ![ExchangeChange]
    , acmpUncompared :: ![UncomparedLine]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ActivityComparison)

{- | Changes as a comparison writes them, to be looked for in an activity: what
a reader proposed against one version, asked of a later one.
-}
data ChangesQuery = ChangesQuery
    { cqSummary :: ![SummaryChange]
    , cqExchanges :: ![ExchangeChange]
    , cqNote :: !(Maybe Text)
    -- ^ Why the changes are applied, kept in the database's history; a question about them ignores it
    , cqSource :: !(Maybe Source)
    -- ^ Where their figures come from, kept beside the reason
    }
    deriving (Generic)
    deriving (FromJSON, ToSchema) via (Stripped ChangesQuery)

{- | Whether an activity already says what one change says.

A line is found the way a comparison pairs it: by its flow and role, else by
its flow's name, compartment and role when that names one line only.
-}
data ChangePresence
    = -- | It says what the change made it say; a removed line is no longer there.
      ChangePresent
    | -- | It still says what the change replaced.
      ChangeAbsent
    | -- | It says something else again.
      ChangeDifferent
    | -- | The line the change is about is not there to judge, or several answer to its name.
      ChangeLineGone
    deriving (Eq, Show, Generic)

instance ToJSON ChangePresence where
    toJSON = \case
        ChangePresent -> "present"
        ChangeAbsent -> "absent"
        ChangeDifferent -> "different"
        ChangeLineGone -> "line-gone"

instance ToSchema ChangePresence where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "ChangePresence") $
                mempty
                    & type_ ?~ OpenApiString
                    & enum_ ?~ ["present", "absent", "different", "line-gone"]

-- | One presence per change, in the order the query listed them.
data ChangesPresence = ChangesPresence
    { cpSummary :: ![ChangePresence]
    , cpExchanges :: ![ChangePresence]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ChangesPresence)

{- | What came of one change asked of an activity to hold: it was applied,
it was there already, or why it was left as the activity says.
-}
data ChangeOutcome
    = -- | The activity says it now.
      OutcomeApplied
    | -- | It said it already.
      OutcomePresent
    | -- | It says something else than what the change replaced: left alone.
      OutcomeDifferent
    | -- | The line the change is about is not there, or several answer to its name.
      OutcomeLineGone
    | -- | An edit cannot say it, for the reason given.
      OutcomeNotApplicable !Text
    deriving (Eq, Show, Generic)

instance ToJSON ChangeOutcome where
    toJSON = \case
        OutcomeApplied -> object ["outcome" .= ("applied" :: Text)]
        OutcomePresent -> object ["outcome" .= ("present" :: Text)]
        OutcomeDifferent -> object ["outcome" .= ("different" :: Text)]
        OutcomeLineGone -> object ["outcome" .= ("line-gone" :: Text)]
        OutcomeNotApplicable reason -> object ["outcome" .= ("not-applicable" :: Text), "reason" .= reason]

instance ToSchema ChangeOutcome where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "ChangeOutcome") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrdHashMap.fromList
                            [ ("outcome", Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ ["applied", "present", "different", "line-gone", "not-applicable"]))
                            , ("reason", Inline (mempty & type_ ?~ OpenApiString))
                            ]
                    & required .~ ["outcome"]

-- | One outcome per change, in the order the query listed them.
data ChangesApplied = ChangesApplied
    { capSummary :: ![ChangeOutcome]
    , capExchanges :: ![ChangeOutcome]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ChangesApplied)

-- | The rung of the cascade that paired two activities of two databases.
data ActivityMatch
    = -- | The same activityUUID_productUUID.
      SameProcessId
    | -- | The same activity and product names, case and a trailing geography aside, at the same location.
      SameNames
    | -- | The same reference product flow at the same location, from the same kind of activity.
      SameProduct
    | -- | The same names cut where SimaPro cuts them, 80 characters with the geography, for a name longer than that.
      SameCutNames
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving anyclass (ToJSON, ToSchema)

data ChangedActivity = ChangedActivity
    { chaMatch :: !ActivityMatch
    , chaComparison :: !ActivityComparison
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ChangedActivity)

-- | Two activities the cascade paired that say the same thing.
data PairedActivities = PairedActivities
    { pamMatch :: !ActivityMatch
    , pamBase :: !ActivitySummary
    , pamOther :: !ActivitySummary
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped PairedActivities)

-- | Activities one key of a rung names on either side, too many to pair.
data AmbiguousActivities = AmbiguousActivities
    { ambMatch :: !ActivityMatch
    , ambBase :: ![ActivitySummary]
    , ambOther :: ![ActivitySummary]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped AmbiguousActivities)

{- | Two databases side by side. The counts always cover the full lists, which
a limit may have truncated. A pair whose only difference is the dates its
datasets state is redated, not changed: a new release restamps every dataset,
and those would bury the few that say something new. The pairs that say the
same thing are listed too, so changed, redated and unchanged together are
every pair the cascade made.
-}
data DatabaseComparison = DatabaseComparison
    { dbcAddedCount :: !Int
    , dbcRemovedCount :: !Int
    , dbcChangedCount :: !Int
    , dbcRedatedCount :: !Int
    , dbcAmbiguousCount :: !Int
    , dbcUnchangedCount :: !Int
    , dbcAdded :: ![ActivitySummary]
    , dbcRemoved :: ![ActivitySummary]
    , dbcChanged :: ![ChangedActivity]
    , dbcRedated :: ![ChangedActivity]
    , dbcAmbiguous :: ![AmbiguousActivities]
    , dbcUnchanged :: ![PairedActivities]
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped DatabaseComparison)

{- | Two activities of two databases the cascade paired, ready for their scores
to be compared: a score of the other activity times 'spPerBaseUnit' is a score
per one unit of the base activity's reference product.
-}
data ScorePair = ScorePair
    { spMatch :: !ActivityMatch
    , spBase :: !Text
    , spOther :: !Text
    , spActivityName :: !Text
    -- ^ As the base database names it.
    , spLocation :: !Text
    , spPerBaseUnit :: !Double
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ScorePair)

{- | Why a score cannot be put per one unit of something else. Two answers use
it: the pairs of two databases, where the base unit is that of the base
database's product, and the scores per a chosen unit, where the base unit is
the one the reader chose and the other is that of a process's product.
-}
data PairRefusal
    = -- | A process has no reference product, so its score is per nothing named.
      NoReferenceProduct
    | -- | The reference product names a unit its own database does not register.
      UnregisteredUnit
    | -- | The unit table does not know this unit.
      UnknownUnit {uuUnit :: !Text}
    | -- | The unit table holds several spellings this unit could be, differing by case.
      AmbiguousUnit {auUnit :: !Text}
    | -- | The two units measure different things: a mass and a volume need a density.
      DifferentMeasures {dmBaseUnit :: !Text, dmOtherUnit :: !Text}
    | -- | The unit table gives this unit a factor of zero, which nothing converts from or to.
      ZeroFactor {zfUnit :: !Text}
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped PairRefusal)

-- | A pair whose scores are not compared, and why.
data SetApartPair = SetApartPair
    { sapBase :: !Text
    , sapOther :: !Text
    , sapActivityName :: !Text
    , sapLocation :: !Text
    , sapReason :: !PairRefusal
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped SetApartPair)

{- | Every pair of activities two databases share, as 'DatabaseComparison'
makes them, without what differs inside each pair: what a client needs to
score both databases and compare the scores activity by activity.
-}
data ScorePairs = ScorePairs
    { spsPairs :: ![ScorePair]
    , spsSetApart :: ![SetApartPair]
    , spsUnpairedBaseCount :: !Int
    , spsUnpairedOtherCount :: !Int
    , spsAmbiguousCount :: !Int
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ScorePairs)

{- | Processes of one database whose scores a reader wants per one unit of
their choosing. The unit is the same for every database of a comparison, so
the caller names it; a misspelt field is refused rather than read as absent.
-}
data ScoreUnitsRequest = ScoreUnitsRequest
    { surUnit :: !Text
    , surProcessIds :: ![Text]
    }
    deriving (Generic)
    deriving (ToSchema) via (Stripped ScoreUnitsRequest)

instance FromJSON ScoreUnitsRequest where
    parseJSON = parseClosed

-- | What a score per one unit of a process's reference product is multiplied by to be per one chosen unit, or why nothing can.
data UnitScale
    = Scaled {usFactor :: !Double}
    | SetApart {usReason :: !PairRefusal}
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped UnitScale)

-- | One asked process, and how its score reads per the chosen unit.
data ScoreUnit = ScoreUnit
    { suProcessId :: !Text
    -- ^ As the caller asked for it, so it joins the batch scores asked with the same id.
    , suActivityName :: !Text
    , suLocation :: !Text
    , suProductUnit :: !(Maybe Text)
    -- ^ Absent when the process has no reference product, or its database registers no unit for it.
    , suScale :: !UnitScale
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ScoreUnit)

-- | How the unit table reads the unit a reader chose.
data ChosenUnit
    = {- | Read. These are the units a reader could choose instead: every one
      the table measures the same quantity in, one spelling per factor,
      smallest first.
      -}
      Understood {cuSameMeasure :: ![Text]}
    | -- | Not read, so every process is set apart under this reason.
      NotUnderstood {cuReason :: !PairRefusal}
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped ChosenUnit)

{- | The asked processes of one database put per one chosen unit. Every asked
id is in exactly one of the three lists, sorted as the batch scores sort them,
so a page joining both answers finds an id in the same place.
-}
data ScoreUnits = ScoreUnits
    { susUnit :: !Text
    -- ^ The table's spelling of the asked unit when it reads it: @KWH@ is answered as @kWh@.
    , susReading :: !ChosenUnit
    , susProcesses :: ![ScoreUnit]
    , susNotFound :: ![Text]
    -- ^ Well-formed ids that name no process of the database.
    , susInvalid :: ![Text]
    -- ^ Ids unusable as sent: malformed, or a bare activity id where the activity has several products.
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ScoreUnits)
