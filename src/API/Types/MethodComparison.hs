{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API.Types.MethodComparison where

import API.JsonOptions (Stripped (..))
import Data.Aeson
import Data.OpenApi (ToSchema (..))
import Data.Text (Text)
import GHC.Generics
import Method.Types (FlowDirection)
import Types (
    UUID,
 )

-- | How two categories of two collections came to be paired.
data CategoryMatch
    = -- | The caller named the pair.
      ForcedByCaller
    | -- | The same method name, case and spacing aside.
      SameMethodName
    | -- | The same impact category, case and spacing aside.
      SameImpactCategory
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving anyclass (ToJSON, ToSchema)

-- | The rung of the cascade that paired two factors of one category.
data FactorMatch
    = -- | The same flow identifier: the one the file writes, or the one the reader derives from the name and compartment when the file writes none.
      SameFlowId
    | -- | The same name, case and punctuation aside, a unit suffix kept.
      SameName
    | -- | Two names of one synonym class, in the view of the factor's direction.
      SameSynonymClass
    | -- | The same CAS number, where the registry does not keep the two names apart.
      SameCAS
    | -- | Two pattern rows, or two exclusion rows, selecting on the same prefix.
      SamePattern
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving anyclass (ToJSON, ToSchema)

-- | How the two values of a paired factor were read before comparing them.
data ValueReading
    = -- | Both factors state the same unit.
      UnitsIdentical
    | -- | Two flow units: the other factor is read per the base factor's unit.
      ConvertedOntoBaseUnit
    | -- | One factor states a flow unit, the other an impact unit: the first is read per its dimension's reference unit, as scoring does.
      ReadPerReferenceUnit
    | -- | Neither unit is a flow unit the unit table knows: compared as written.
      ComparedAsWritten
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving anyclass (ToJSON, ToSchema)

-- | A factor's direction, spelt @Input@ or @Output@ as the factors of a method are.
deriving anyclass instance ToSchema FlowDirection

-- | One factor as its collection writes it.
data FactorSide = FactorSide
    { facFlowName :: !Text
    , facDirection :: !FlowDirection
    , facCompartment :: !Text
    -- ^ medium/subcompartment/qualifier, empty parts left out, as written
    , facCas :: !(Maybe Text)
    , facLocation :: !(Maybe Text)
    , facUnit :: !(Maybe Text)
    -- ^ absent when the method states none
    , facValue :: !Double
    , facFlowRef :: !UUID
    -- ^ what a change to this factor addresses; last, so factors still sort by name
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped FactorSide)

data ChangedFactor = ChangedFactor
    { cfxMatch :: !FactorMatch
    , cfxBase :: !FactorSide
    , cfxOther :: !FactorSide
    , cfxReading :: !ValueReading
    , cfxRatio :: !(Maybe Double)
    -- ^ other over base once both are read per one unit; absent when the base is zero
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped ChangedFactor)

-- | Two factors paired on their key whose values cannot be read per one unit: two flow units that do not convert, or a spelling the unit table cannot settle.
data UnconvertibleFactor = UnconvertibleFactor
    { ufxMatch :: !FactorMatch
    , ufxBase :: !FactorSide
    , ufxOther :: !FactorSide
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped UnconvertibleFactor)

-- | Factors one key of a rung names on either side, too many to pair.
data AmbiguousFactors = AmbiguousFactors
    { afxMatch :: !FactorMatch
    , afxBase :: ![FactorSide]
    , afxOther :: ![FactorSide]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped AmbiguousFactors)

-- | A category of one collection, as the comparison names it.
data CategorySide = CategorySide
    { csdName :: !Text
    , csdCategory :: !Text
    , csdUnit :: !Text
    , csdFactorCount :: !Int
    , csdMethodId :: !UUID
    -- ^ what a change to one of this category's factors addresses
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped CategorySide)

-- | Two paired categories, factor by factor. The counts cover the full lists, which a limit may have truncated.
data CategoryComparison = CategoryComparison
    { ccpMatch :: !CategoryMatch
    , ccpBase :: !CategorySide
    , ccpOther :: !CategorySide
    , ccpAddedCount :: !Int
    , ccpRemovedCount :: !Int
    , ccpChangedCount :: !Int
    , ccpUnchangedCount :: !Int
    , ccpAmbiguousCount :: !Int
    , ccpUnconvertibleCount :: !Int
    , ccpLargestRatio :: !(Maybe Double)
    -- ^ the ratio of the change that leads the list: a sign flip, a vanished factor (0) or one appeared from zero (absent) first, then the farthest from one
    , ccpAdded :: ![FactorSide]
    , ccpRemoved :: ![FactorSide]
    , ccpChanged :: ![ChangedFactor]
    , ccpAmbiguous :: ![AmbiguousFactors]
    , ccpUnconvertible :: ![UnconvertibleFactor]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped CategoryComparison)

-- | Categories one key names on either side, too many to pair.
data AmbiguousCategories = AmbiguousCategories
    { acgMatch :: !CategoryMatch
    , acgBase :: ![CategorySide]
    , acgOther :: ![CategorySide]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped AmbiguousCategories)

-- | Two method collections side by side.
data MethodCollectionComparison = MethodCollectionComparison
    { mccCategories :: ![CategoryComparison]
    , mccUnpairedBase :: ![CategorySide]
    , mccUnpairedOther :: ![CategorySide]
    , mccAmbiguous :: ![AmbiguousCategories]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped MethodCollectionComparison)

-- | How many factors of a category one medium holds; no medium for the factors filed under none.
data MediumCount = MediumCount
    { mdcMedium :: !(Maybe Text)
    , mdcFactorCount :: !Int
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped MediumCount)

-- | Where a factor's location was read.
data LocationSource
    = -- | The location field the file writes.
      InField
    | -- | The end of the name (@Ammonia, FR@), the file writing no location.
      InName
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving anyclass (ToJSON, ToSchema)

data ReadLocation = ReadLocation
    { rlCode :: !Text
    , rlFrom :: !LocationSource
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped ReadLocation)

{- | A factor's medium and location as a comparison of two collections and a
collection's profile read them: the medium after the compartment table, no
medium for a factor filed under none, and no location for one written for
none.
-}
data FactorReading = FactorReading
    { frMedium :: !(Maybe Text)
    , frLocation :: !(Maybe ReadLocation)
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped FactorReading)

-- | Factors of one category that one key answers to, at one place: a comparison could pair none of them.
data DuplicateFactors = DuplicateFactors
    { dfxMatch :: !FactorMatch
    , dfxFactors :: ![FactorSide]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped DuplicateFactors)

-- | What one impact category holds, read without listing its factors.
data CategoryProfile = CategoryProfile
    { cpfCategory :: !CategorySide
    , cpfMedia :: ![MediumCount]
    , cpfLocatedCount :: !Int
    -- ^ factors written for one location, stated or read at the end of the name
    , cpfLocatedInNameCount :: !Int
    -- ^ of the located factors, those whose name carries the location
    , cpfLocationCount :: !Int
    , cpfZeroCount :: !Int
    , cpfPatternCount :: !Int
    -- ^ pattern and exclusion rows, which select flows rather than name one
    , cpfDuplicates :: ![DuplicateFactors]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped CategoryProfile)

-- | What each impact category of a method collection holds.
newtype MethodCollectionProfile = MethodCollectionProfile
    { mcpCategories :: [CategoryProfile]
    }
    deriving (Eq, Show, Generic)
    deriving (ToJSON, ToSchema) via (Stripped MethodCollectionProfile)
