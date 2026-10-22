{-# LANGUAGE OverloadedStrings #-}

{- | Relink-with-mapping: re-link an (unlinked) database to a chosen background
dependency using a curated alias mapping loaded from a CSV.

A consumer database often carries background inputs named after one database
while the replacement dependency names the same activities differently. A
curated CSV maps the source (consumer) input to the target supplier, so the
cross-DB matcher resolves the link even though the raw names differ. A row
preempts the direct name cascade – the curator's designation always wins over
a coincidental direct match.

The CSV is header-based (cassava 'decodeByName'); columns it does not know are
ignored, so a file can carry notes for the people who read it. Recognized
columns:

  * @source_product@ (or @source@ / @source_name@ / @from@): the consumer's
    input flow name
  * @source_id@: the supplier process the consumer's input names by identity
    (@activityUUID_productUUID@), as an EcoSpold 2 input does; a row with one
    applies to such an input before any row keyed by name
  * @source_location@ (or @source_geo@): restrict the row to demands at this
    exact location code; a row without one applies at any location, and an
    exact (name, location) row wins over the name-only row for the same name
  * @target_product@ (or @target@ / @target_name@ / @to@): the supplier
    product name
  * @target_id@: the supplier process itself, which wins over its name
  * @target_location@ (or @target_geo@): pin the supplier to this exact
    location code, bypassing the geography policy; when nothing supplies the
    target name there, the relink reports 'Types.AliasTargetMissing' rather
    than silently falling back
  * @conversion@, with @source_unit@ and @target_unit@: one target unit in
    source units (a car of 1200 kg is 1 p: 1200, kg, p). Used only where the
    demand's unit and the supplier's measure different things; units of one
    measure convert by the unit table, as they do without a mapping.
  * @direction@: @opposite@ when the target counts the demand the other way
    than the supplier the line was written for, @same@ (or blank) otherwise.
    A line of -0.018 m3 to a wastewater market whose reference is -1 m3 asks
    for 0.018 m3 treated; mapped to a treatment whose reference is +1 m3, the
    row says @opposite@ and the line becomes +0.018, or the treatment would
    run backwards and score a credit. Independent of @conversion@: a row may
    carry both. Only a database whose lines are read as written (EcoSpold 2,
    openLCA, Brightway) is turned by it: a SimaPro, ILCD or EcoSpold 1 line
    already takes its supplier's sign, so there the column changes nothing,
    and one mapping serves a source of either kind.

A row needs a source (name or identity). With a target (name or identity) it
is a link; without one it declines the link: its input stays unlinked, reported
as 'Types.AliasDeclined', where it would otherwise meet the usual matching. The
file is the curator's word on every input it names; an input it does not name
meets the usual matching. A target without a source is a mistake, refused with
its row number.

Rows that give one key different targets do not fail the file: each demand
under that key is reported as 'Types.AliasConflict', and the rest applies.
Unit-incompatible or consumed-nowhere outcomes are not silently dropped either –
they surface through the relink's 'CrossDBLinkingStats' (unresolved products
carry a 'LinkBlocker') and through the returned 'RelinkResult'.
-}
module Database.RelinkMapping (
    AliasRow (..),
    parseAliasCSV,
    buildAliasMap,
    parseAliasMap,
) where

import Control.Applicative ((<|>))
import Control.Monad (mfilter)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromNamedRecord (..), (.:))
import qualified Data.Csv as Csv
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector as V

import Database.CrossLinking (AliasKey (..), AliasMap (..), AliasSupplier (..), AliasTarget (..), Direction (..), UnitBridge (..))
import Types (ProcessRef, parseProcessRef, processRefText)

{- | One row of the alias mapping, validated: the keys it applies under (at
least one of a source name and a source identity) and the supplier it
designates. The source location scopes the name key to demands at that exact
code – see the module header for the full semantics.
-}
data AliasRow = AliasRow
    { arSource :: !(Maybe Text)
    , arSourceLocation :: !(Maybe Text)
    , arSourceProcess :: !(Maybe ProcessRef)
    , arTarget :: !AliasTarget
    }
    deriving (Show, Eq)

-- | A row as written, every cell optional: 'classify' says what it means.
data WrittenRow = WrittenRow
    { wrSource :: !(Maybe Text)
    , wrSourceLocation :: !(Maybe Text)
    , wrSourceId :: !(Maybe Text)
    , wrSourceUnit :: !(Maybe Text)
    , wrTarget :: !(Maybe Text)
    , wrTargetLocation :: !(Maybe Text)
    , wrTargetId :: !(Maybe Text)
    , wrTargetUnit :: !(Maybe Text)
    , wrConversion :: !(Maybe Text)
    , wrDirection :: !(Maybe Text)
    }

sourceNames, targetNames :: [Text]
sourceNames = ["source_product", "source", "source_name", "from"]
targetNames = ["target_product", "target", "target_name", "to"]

instance FromNamedRecord WrittenRow where
    parseNamedRecord r =
        WrittenRow
            <$> optField r sourceNames
            <*> optField r ["source_location", "source_geo"]
            <*> optField r ["source_id"]
            <*> optField r ["source_unit"]
            <*> optField r targetNames
            <*> optField r ["target_location", "target_geo"]
            <*> optField r ["target_id"]
            <*> optField r ["target_unit"]
            <*> optField r ["conversion"]
            <*> optField r ["direction"]

{- | Parse the first present column among @names@ as optional. A present but
blank (whitespace-only) cell normalizes to 'Nothing' so "absent" has a single
representation before any consumer relies on it.
-}
optField :: Csv.NamedRecord -> [Text] -> Csv.Parser (Maybe Text)
optField r =
    foldr (\nm acc -> (blankToNothing <$> r .: Csv.toField nm) <|> acc) (pure Nothing)
  where
    blankToNothing = mfilter (not . T.null) . Just . T.strip

{- | Parse alias rows from CSV bytes. The header must name a source column and
a target column (by name or identity). A row with neither source nor target is
dropped; a row that cannot be read as written is refused with its number
rather than skipped.
-}
parseAliasCSV :: BL.ByteString -> Either Text [AliasRow]
parseAliasCSV bytes =
    case Csv.decodeByName bytes of
        Left err -> Left $ "alias CSV parse error: " <> T.pack err
        Right (header, rows) -> do
            requireColumn header "source" ("source_id" : sourceNames)
            requireColumn header "target" ("target_id" : targetNames)
            catMaybes <$> traverse (uncurry classify) (zip [1 :: Int ..] (V.toList rows))
  where
    requireColumn header side names
        | any ((`V.elem` header) . Csv.toField) names = Right ()
        | otherwise =
            Left $ "missing required " <> side <> " column (one of: " <> T.intercalate ", " names <> ")"

-- | What row @n@ says: a link, a declined link, nothing, or a mistake to refuse.
classify :: Int -> WrittenRow -> Either Text (Maybe AliasRow)
classify n row = do
    targetProcess <- traverse (readRef "target_id") (wrTargetId row)
    let supplier = case (targetProcess, wrTarget row) of
            (Just ref, label) -> Just (SupplierProcess ref (fromMaybe (processRefText ref) label))
            (Nothing, Just name) -> Just (SupplierNamed name (wrTargetLocation row))
            (Nothing, Nothing) -> Nothing
    case (hasSource, supplier) of
        (False, Nothing) -> Right Nothing
        (False, Just _) -> refuse "has a target but no source"
        -- A declined link converts nothing: its conversion and direction, if any, are not read.
        (True, Nothing) -> keyed (AliasTarget NoSupplier Nothing Same)
        (True, Just target) -> keyed =<< AliasTarget target <$> traverse readBridge (wrConversion row) <*> readDirection (wrDirection row)
  where
    keyed target = do
        sourceProcess <- traverse (readRef "source_id") (wrSourceId row)
        Right . Just $
            AliasRow
                { arSource = wrSource row
                , arSourceLocation = wrSourceLocation row
                , arSourceProcess = sourceProcess
                , arTarget = target
                }
    hasSource = isJust (wrSource row) || isJust (wrSourceId row)
    refuse why = Left $ "alias CSV row " <> T.pack (show n) <> ": " <> why
    readRef column txt =
        maybe (refuse (column <> " is not activityUUID_productUUID: " <> txt)) Right (parseProcessRef txt)
    readBridge txt = case (TR.double txt, wrSourceUnit row, wrTargetUnit row) of
        (Right (value, rest), Just sourceUnit, Just targetUnit)
            | T.null rest && value > 0 -> Right (UnitBridge sourceUnit targetUnit value)
        (_, Just _, Just _) -> refuse ("conversion must be a positive number: " <> txt)
        _ -> refuse "conversion needs source_unit and target_unit, to say which way it reads"
    readDirection cell = case T.toLower <$> cell of
        Nothing -> Right Same
        Just "same" -> Right Same
        Just "opposite" -> Right Opposite
        Just other -> refuse ("direction must be same or opposite: " <> other)

{- | Build the alias map, keyed by source identity and by (source name, optional
source location). The same name with and without a location is two distinct
keys (the located one wins at lookup). A key keeps every distinct target its
rows gave it; identical duplicates collapse.

A row on a process that names no supplier declines that process's link, not
its name's: a mapping page writes one row per process, and the twenty
producers of "electricity, high voltage" at CH left undecided must not
overrule, for a demand by name, the market a person linked.
-}
buildAliasMap :: [AliasRow] -> AliasMap
buildAliasMap rows =
    AliasMap
        { amByProcess = keyed [(ref, arTarget row) | row <- rows, Just ref <- [arSourceProcess row]]
        , amByName = keyed [(AliasKey name (arSourceLocation row), arTarget row) | row <- rows, speaksForName row, Just name <- [arSource row]]
        }
  where
    speaksForName row = isNothing (arSourceProcess row) || atSupplier (arTarget row) /= NoSupplier
    keyed :: (Ord k) => [(k, AliasTarget)] -> M.Map k (NE.NonEmpty AliasTarget)
    keyed pairs = M.fromListWith (\new old -> NE.nub (old <> new)) [(k, pure t) | (k, t) <- pairs]

{- | Read a relink mapping. One with no row (header only) would relink as a
silent no-op, so it is refused.
-}
parseAliasMap :: BL.ByteString -> Either Text AliasMap
parseAliasMap bytes = do
    m <- buildAliasMap <$> parseAliasCSV bytes
    if M.null (amByProcess m) && M.null (amByName m)
        then Left "mapping file contains no row with a source"
        else Right m
