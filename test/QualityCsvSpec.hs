{-# LANGUAGE OverloadedStrings #-}

{- | The CSV rendering of the quality reports.

Two things can go wrong here without breaking the build: a check missing from
the render, which drops findings from a file that still looks complete, and a
cell reaching a spreadsheet as a formula instead of as the text the database
holds. Both are pinned below.
-}
module QualityCsvSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isInfixOf)
import Data.Text (Text)
import Test.Hspec

import API.Csv (computedQualityReportCsv, qualityReportCsv)
import API.Types (ComputedQualityReportAPI (..), QualityCheckAPI (..), QualityOffenderAPI (..), QualityReportAPI (..))
import Types (Severity (..))

-- | A check holding one finding, carrying the given detail.
finding :: Text -> QualityCheckAPI
finding detail =
    QualityCheckAPI
        { qcaApplicable = True
        , qcaOffenderCount = 1
        , qcaOffenders =
            [ QualityOffenderAPI
                { qoaSeverity = WarningSev
                , qoaProcessId = "act_prod"
                , qoaActivityName = "wheat production"
                , qoaLocation = "FR"
                , qoaProductName = Just "wheat grain"
                , qoaDetail = detail
                }
            ]
        }

-- | A check with nothing to report.
quiet :: QualityCheckAPI
quiet = QualityCheckAPI{qcaApplicable = True, qcaOffenderCount = 0, qcaOffenders = []}

{- | Every structural check carries one finding, so the rendered rows say
which checks were reached and in what order.
-}
everyCheck :: QualityReportAPI
everyCheck =
    QualityReportAPI
        { qraDbName = "agb32"
        , qraProcessCount = 1
        , qraReferenceProduct = finding "reference product"
        , qraAllocationSums = finding "allocation sums"
        , qraDuplicateActivities = finding "duplicate activities"
        , qraDuplicateProducts = finding "duplicate products"
        , qraSuspiciousAmounts = finding "suspicious amounts"
        , qraMissingMetadata = finding "missing metadata"
        , qraUndeclaredGeography = finding "undeclared geography"
        , qraFormulaConsistency = finding "formula consistency"
        , qraTruncatedNameCollisions = finding "truncated name collisions"
        , qraMissingPedigree = finding "missing pedigree"
        , qraUnconsumedProducts = finding "unconsumed products"
        , qraUnsuppliedInputs = finding "unsupplied inputs"
        , qraObsoleteInputs = finding "obsolete inputs"
        , qraLandTransformationBalance = finding "land transformation balance"
        , qraOxygenDemandOrder = finding "oxygen demand order"
        , qraInvalidCas = finding "invalid cas"
        , qraAllocationOutOfRange = finding "allocation out of range"
        , qraUnallocated = finding "unallocated"
        , qraUnmeasurableAmounts = finding "unmeasurable amounts"
        , qraWaterBalance = finding "water balance"
        }

-- | The structural report with exactly one check reporting the given detail.
oneFinding :: Text -> QualityReportAPI
oneFinding detail =
    QualityReportAPI
        { qraDbName = "agb32"
        , qraProcessCount = 1
        , qraReferenceProduct = finding detail
        , qraAllocationSums = quiet
        , qraDuplicateActivities = quiet
        , qraDuplicateProducts = quiet
        , qraSuspiciousAmounts = quiet
        , qraMissingMetadata = quiet
        , qraUndeclaredGeography = quiet
        , qraFormulaConsistency = quiet
        , qraTruncatedNameCollisions = quiet
        , qraMissingPedigree = quiet
        , qraUnconsumedProducts = quiet
        , qraUnsuppliedInputs = quiet
        , qraObsoleteInputs = quiet
        , qraLandTransformationBalance = quiet
        , qraOxygenDemandOrder = quiet
        , qraInvalidCas = quiet
        , qraAllocationOutOfRange = quiet
        , qraUnallocated = quiet
        , qraUnmeasurableAmounts = quiet
        , qraWaterBalance = quiet
        }

-- | A report whose every check passed.
nothingWrong :: QualityReportAPI
nothingWrong = (oneFinding "unused"){qraReferenceProduct = quiet}

{- | A finding whose activity and product open the way a spreadsheet formula
does, which the parser is entitled to produce from a database that holds them.
-}
namedLikeAFormula :: QualityReportAPI
namedLikeAFormula =
    let offender o =
            o
                { qoaActivityName = "=trouble"
                , qoaLocation = "-1"
                , qoaProductName = Just "+additive"
                }
        check = finding "plain"
     in (oneFinding "plain"){qraReferenceProduct = check{qcaOffenders = map offender (qcaOffenders check)}}

computed :: ComputedQualityReportAPI
computed =
    ComputedQualityReportAPI
        { cqaDbName = "agb32"
        , cqaCollection = "EF31"
        , cqaProcessCount = 1
        , cqaScoreOutliers = finding "score outliers"
        , cqaZeroScores = quiet
        , cqaNegativeScores = finding "negative scores"
        }

-- | The rendered lines, CRLF and the trailing blank dropped.
rows :: BL.ByteString -> [String]
rows = filter (not . null) . lines . filter (/= '\r') . BL.unpack

-- | The first cell of each line: the name of the check that raised the row.
checkColumn :: BL.ByteString -> [String]
checkColumn = map (takeWhile (/= ',')) . rows

spec :: Spec
spec = describe "Quality report CSV" $ do
    it "opens with the header, whatever the report holds" $
        take 1 (rows (qualityReportCsv (oneFinding "x")))
            `shouldBe` ["check,severity,activity_name,location,product_name,detail,process_id"]

    it "renders one row per finding, every check in report order" $
        checkColumn (qualityReportCsv everyCheck)
            `shouldBe` [ "check"
                       , "reference_product"
                       , "allocation_sums"
                       , "duplicate_activities"
                       , "duplicate_products"
                       , "suspicious_amounts"
                       , "missing_metadata"
                       , "undeclared_geography"
                       , "formula_consistency"
                       , "truncated_name_collisions"
                       , "missing_pedigree"
                       , "unconsumed_products"
                       , "unsupplied_inputs"
                       , "obsolete_inputs"
                       , "land_transformation_balance"
                       , "oxygen_demand_order"
                       , "invalid_cas"
                       , "allocation_out_of_range"
                       , "unallocated"
                       , "unmeasurable_amounts"
                       , "water_balance"
                       ]

    it "gives a check with nothing to report no row of its own" $
        checkColumn (qualityReportCsv (oneFinding "the only one"))
            `shouldBe` ["check", "reference_product"]

    it "writes nothing but the header for a database with nothing wrong" $
        rows (qualityReportCsv nothingWrong)
            `shouldBe` ["check,severity,activity_name,location,product_name,detail,process_id"]

    it "writes the whole finding on its row" $
        drop 1 (rows (qualityReportCsv (oneFinding "no reference product")))
            `shouldBe` ["reference_product,warning,wheat production,FR,wheat grain,no reference product,act_prod"]

    it "quotes a detail holding a comma rather than splitting the row" $
        rows (qualityReportCsv (oneFinding "sums to 0.7, expected 1.0"))
            `shouldSatisfy` any ("\"sums to 0.7, expected 1.0\"" `isInfixOf`)

    it "keeps a detail opening with a minus sign from reading as a formula" $
        drop 1 (rows (qualityReportCsv (oneFinding "-1.5 kg where a product is expected")))
            `shouldBe` ["reference_product,warning,wheat production,FR,wheat grain, -1.5 kg where a product is expected,act_prod"]

    it "guards every cell carrying database content, location included" $
        drop 1 (rows (qualityReportCsv namedLikeAFormula))
            `shouldBe` ["reference_product,warning, =trouble, -1, +additive,plain,act_prod"]

    it "renders the computed report with the same columns and its own checks" $
        checkColumn (computedQualityReportCsv computed)
            `shouldBe` ["check", "score_outliers", "negative_scores"]
