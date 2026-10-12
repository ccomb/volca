{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types.Comparison where

import API.JsonOptions (Stripped (..))
import API.Types.Common
import Control.Lens ((&), (.~), (?~))
import Data.Aeson
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
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
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving anyclass (ToJSON, ToSchema)

data ChangedActivity = ChangedActivity
    { chaMatch :: !ActivityMatch
    , chaComparison :: !ActivityComparison
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped ChangedActivity)

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
and those would bury the few that say something new.
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
    }
    deriving (Generic)
    deriving (ToJSON, ToSchema) via (Stripped DatabaseComparison)
