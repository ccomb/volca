{-# LANGUAGE OverloadedStrings #-}

{- | A database's processes as a whole, for a client that indexes them, and a
fingerprint that changes exactly when what it lists does.

The fingerprint hashes the encoded entries themselves, so it moves on a renamed
process, an edited comment or a unit the table now reads differently, and
stays put across an edit that touches none of them (an exchange amount).
-}
module Service.Catalogue (
    measureOf,
    catalogueEntries,
    catalogueFingerprint,
    cataloguePage,
    catalogueDefaultLimit,
    catalogueMaxLimit,
) where

import Data.Aeson (encode)
import Data.Bits (xor)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word64, Word8)
import Text.Printf (printf)

import API.Types (CatalogueEntry (..), CatalogueMeasure (..), CataloguePage (..))
import Service (ReferenceProductInfo (..), referenceProductOf, summaryProduct)
import Types (Activity (..), Database (..), processIdToText)
import UnitConversion (UnitConfig, UnitDef (..), UnitReading (..), readUnit)

catalogueDefaultLimit :: Int
catalogueDefaultLimit = 1000

-- | A page of 5000 entries with their comments stays within a few MB.
catalogueMaxLimit :: Int
catalogueMaxLimit = 5000

measureOf :: UnitConfig -> Text -> Maybe CatalogueMeasure
measureOf cfg unit = case readUnit cfg unit of
    ReadExact def -> Just (measure def)
    ReadRespelt _ def -> Just (measure def)
    ReadAmbiguous{} -> Nothing
    ReadUnknown -> Nothing
  where
    measure :: UnitDef -> CatalogueMeasure
    measure def = CatalogueMeasure (udDimension def) (udFactor def)

catalogueEntries :: UnitConfig -> Database -> [CatalogueEntry]
catalogueEntries cfg db = V.toList (V.imap entry (dbActivities db))
  where
    entry :: Int -> Activity -> CatalogueEntry
    entry i activity =
        let refProduct = summaryProduct (referenceProductOf (dbTechFlows db) (dbUnits db) activity)
         in CatalogueEntry
                { ceProcessId = processIdToText db (fromIntegral i)
                , ceActivityName = activityName activity
                , ceProductName = rpName refProduct
                , ceLocation = activityLocation activity
                , ceUnit = rpUnit refProduct
                , ceMeasure = measureOf cfg (rpUnit refProduct)
                , ceClassification = activityClassification activity
                , ceDescription = activityDescription activity
                }

{- | FNV-1a over the encoded catalogue. It tells a copy that went stale, it does
not stand against someone forging a collision, and it reads the bytes as they
are: going through a 'String' first cost seconds on a large database.
-}
catalogueFingerprint :: [CatalogueEntry] -> Text
catalogueFingerprint = T.pack . printf "%016x" . BL.foldl' step 14695981039346656037 . encode
  where
    step :: Word64 -> Word8 -> Word64
    step h byte = (h `xor` fromIntegral byte) * 1099511628211

cataloguePage :: [CatalogueEntry] -> Int -> Int -> Either Text CataloguePage
cataloguePage entries offset limit
    | offset < 0 = Left "offset must be zero or more"
    | limit < 1 = Left "limit must be at least 1"
    | limit > catalogueMaxLimit = Left ("limit must be at most " <> T.pack (show catalogueMaxLimit))
    | otherwise =
        Right
            CataloguePage
                { cpFingerprint = catalogueFingerprint entries
                , cpTotal = length entries
                , cpOffset = offset
                , cpEntries = take limit (drop offset entries)
                }
