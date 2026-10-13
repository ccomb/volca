{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The @text/csv@ representation of the quality reports.

One report, two representations: the JSON route answers a program, this one
answers a spreadsheet. Every surface that hands the report over as a file
reads this module, so the columns and the quoting have one definition rather
than one per client.

The two reports share a row shape, so they share an encoder: one row per
finding, the name of the check that raised it in the first column.
-}
module API.Csv (
    CSV,
    qualityReportCsv,
    computedQualityReportCsv,
    spreadsheetSafe,
) where

import API.Types (ComputedQualityReportAPI (..), QualityCheckAPI (..), QualityOffenderAPI (..), QualityReportAPI (..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (..), MimeRender (..))
import Types (severityCode)

-- | The @text/csv@ content type.
data CSV

instance Accept CSV where
    contentType _ = "text" // "csv" /: ("charset", "utf-8")

instance MimeRender CSV QualityReportAPI where
    mimeRender _ = qualityReportCsv

instance MimeRender CSV ComputedQualityReportAPI where
    mimeRender _ = computedQualityReportCsv

{- | The structural report as CSV, one row per finding, checks in report order.
The check names are snake_case where the JSON's keys are camelCase
(@reference_product@ against @referenceProduct@): a spreadsheet column heading
is read by a person, and these names are the ones the reports have been
downloaded under since they existed.
-}
qualityReportCsv :: QualityReportAPI -> BL.ByteString
qualityReportCsv r =
    checksCsv
        [ ("reference_product", qraReferenceProduct r)
        , ("allocation_sums", qraAllocationSums r)
        , ("duplicate_activities", qraDuplicateActivities r)
        , ("duplicate_products", qraDuplicateProducts r)
        , ("suspicious_amounts", qraSuspiciousAmounts r)
        , ("missing_metadata", qraMissingMetadata r)
        , ("undeclared_geography", qraUndeclaredGeography r)
        , ("formula_consistency", qraFormulaConsistency r)
        , ("truncated_name_collisions", qraTruncatedNameCollisions r)
        , ("missing_pedigree", qraMissingPedigree r)
        , ("unconsumed_products", qraUnconsumedProducts r)
        , ("unsupplied_inputs", qraUnsuppliedInputs r)
        , ("obsolete_inputs", qraObsoleteInputs r)
        , ("land_transformation_balance", qraLandTransformationBalance r)
        , ("oxygen_demand_order", qraOxygenDemandOrder r)
        , ("invalid_cas", qraInvalidCas r)
        , ("allocation_out_of_range", qraAllocationOutOfRange r)
        , ("unallocated", qraUnallocated r)
        , ("unmeasurable_amounts", qraUnmeasurableAmounts r)
        , ("water_balance", qraWaterBalance r)
        ]

-- | The computed report as CSV: same columns, same guards, its own checks.
computedQualityReportCsv :: ComputedQualityReportAPI -> BL.ByteString
computedQualityReportCsv r =
    checksCsv
        [ ("score_outliers", cqaScoreOutliers r)
        , ("zero_scores", cqaZeroScores r)
        , ("negative_scores", cqaNegativeScores r)
        ]

{- | The findings of a report as rows, checks in report order. A check with
nothing to report contributes no row; the header is always written, so a
database with nothing wrong yields a header and no rows rather than an empty
file the caller has to guess about.
-}
checksCsv :: [(Text, QualityCheckAPI)] -> BL.ByteString
checksCsv namedChecks =
    Csv.encode (header : concatMap checkRows namedChecks)
  where
    header = ["check", "severity", "activity_name", "location", "product_name", "detail", "process_id"]
    checkRows (name, check) = map (offenderRow name) (qcaOffenders check)
    offenderRow name o =
        [ name
        , severityCode (qoaSeverity o)
        , spreadsheetSafe (qoaActivityName o)
        , spreadsheetSafe (qoaLocation o)
        , spreadsheetSafe (fromMaybe "" (qoaProductName o))
        , spreadsheetSafe (qoaDetail o)
        , qoaProcessId o
        ]

{- | A cell opening with @=@, @+@, @-@, @\@@ or a tab is read as a formula by
spreadsheets, and an allocation detail legitimately opens with a minus sign.
A leading space keeps it text. Applied to every cell carrying database
content - a location is as free-form as a name once a parser has read it -
and to no other, because quoting must not alter data the caller chose.
-}
spreadsheetSafe :: Text -> Text
spreadsheetSafe t
    | any (`T.isPrefixOf` t) ["=", "+", "-", "@", "\t", "\r"] = T.cons ' ' t
    | otherwise = t
