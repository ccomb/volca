{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | Format dispatch for database export.

Inverts the upload/parse path: given an in-memory 'Database', serialize it to
bytes in one of the supported formats by delegating to the per-format writer.

Single-file formats (SimaPro CSV, EcoSpold 1, Brightway Excel) serialize to one
byte stream. Multi-file formats (EcoSpold 2, ILCD, an openLCA package) are inherently
directory trees, so they are packaged into a deterministic zip archive.
'OpenLcaImpactCategory' has no database writer (methods have their own) and
'UnknownFormat' is not a real target, so both fail loudly ('Left') rather than
emit a silent empty file.
-}
module Database.Export (
    serializeDatabase,
    namedAfterLinks,
    MethodExportFormat (..),
    parseMethodExportFormat,
    serializeMethodCollection,
    exportMethodCollection,
    parseExportFormat,
) where

import Control.Exception (SomeException, try)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified BrightwayExcel.Writer as BE
import Database.ExportFormulas (valuesOnly)
import Database.ExportSupplier (Dependencies (..), Elsewhere (..))
import Database.Loader (lineInLinkUnit)
import Database.Upload (DatabaseFormat (..), referenceConventionOf)
import qualified EcoSpold.Writer1 as ES1
import qualified EcoSpold.Writer2 as ES2
import qualified ILCD.Writer as ILCD
import Method.Mapping (isExclusionCF)
import Method.Types (Method (..), MethodCollection (..))
import qualified Method.WriterCSV as MWC
import qualified Method.WriterILCD as MWI
import qualified Method.WriterOlcaSchema as MWO
import qualified Method.WriterSimaPro as MW
import qualified OlcaSchema.Writer as OLCA
import SimaPro.Parser (generateUnitUUID)
import qualified SimaPro.Writer as SP
import Types (
    Activity (..),
    AnsweredExchange (..),
    CrossDBLink (..),
    Database (..),
    Exchange (..),
    ReferenceConvention (..),
    ScenarioValue,
    SimpleDatabase (..),
    TechnosphereFlow (..),
    Unit (..),
    readingSign,
    toSimpleDatabase,
 )
import UnitConversion (UnitConfig)
import Zip (zipFiles)

{- | Serialize a database to a single byte stream in the requested format, paired
with any best-effort approximation warnings. Pure: the multi-file formats are
zipped in-memory. Fails loudly for formats without a writer. The saved
scenarios given, by name, are written by an openLCA package alone, on its
product systems.

The warning list is empty for a faithful export and non-empty when a writer had
to approximate. Three writers approximate today (an openLCA package as ILCD does,
plus what 'OlcaSchema.Writer.serializeOlcaPackage' lists): Brightway has no waste type, so
it rewrites /orphan/ waste exchanges as technosphere flows (inventory-neutral,
but the waste tag is lost on re-import); ILCD keys one process per dataset UUID,
so a multi-output activity's products export as separate, unlinked datasets
('ILCD.Writer.splitWarnings'). Returning bytes and warnings together shares the
one 'toSimpleDatabase' conversion and keeps them from drifting apart.

The dependencies are the suppliers its inputs buy from the databases it
depends on ('Database.ExportSupplier.dependenciesOf'). SimaPro and Brightway
name them on the rows buying from them; ILCD and EcoSpold 1, which cannot,
refuse them. Only those four read them, so only they fail when the suppliers
could not be worked out: the formats pointing at a supplier export all the
same, under the names the source gave. Before any writer, each input a dependency supplies is
written under that supplier's product and identity ('namedAfterLinks').
-}
serializeDatabase :: UnitConfig -> DatabaseFormat -> M.Map Text [ScenarioValue] -> Either Text Dependencies -> Database -> Either Text (BL.ByteString, [Text])
serializeDatabase units fmt saved dependencies db = fmap (fmap (linkWarnings <>)) $ case fmt of
    -- Each writer runs its own check*Exportable and returns 'Left' on a database
    -- the format cannot represent faithfully, so the guard is unskippable.
    SimaProCSV -> dependencies >>= \deps -> first BL.fromStrict <$> SP.serializeSimaProCSVWithWarnings SP.defaultWriterConfig{SP.wcDependencies = deps} sdb
    -- One lazy chunk per dataset, never one value holding the file: see
    -- 'ES1.writeSimpleDatabaseChunks' for what a single chunk costs at this size.
    EcoSpold1 ->
        dependencies >>= \(Dependencies found) -> case M.elems found of
            supplier : _ -> Left ("EcoSpold 1 names a supplier only among the datasets of its own file, and cannot for an input bought from " <> elDatabase supplier <> ". EcoSpold 2 and openLCA point at suppliers instead, and can.")
            [] -> valuesOnlyWith [] (BL.fromChunks . map TE.encodeUtf8 <$> ES1.writeSimpleDatabaseChunks ES1.canonicalWriterOptions sdb)
    EcoSpold2 -> first zipText <$> ES2.writeEcoSpold2WithWarnings ES2.noVolatileMeta sdb
    ILCDProcess -> dependencies >>= \deps -> valuesOnlyWith (ILCD.splitWarnings sdb) (ILCD.writeILCDArchive ILCD.defaultWriteOptions{ILCD.woDependencies = deps} sdb)
    BrightwayExcel -> dependencies >>= \deps -> BE.renderWorkbookWithWarnings BE.defaultWriterConfig{BE.wcDependencies = deps} sdb
    OpenLcaImpactCategory ->
        Left "openLCA JSON-LD export is not supported"
    ColumnarCSV ->
        Left "a columnar CSV holds methods, not a database"
    OpenLcaPackage -> first zipFiles <$> OLCA.serializeOlcaPackage units saved sdb
    UnknownFormat ->
        Left "cannot export to an unknown format"
  where
    (sdb, linkWarnings) = case dependencies of
        Right deps -> namedAfterLinks units (referenceConventionOf fmt) deps (dbCrossDBLinks db) (toSimpleDatabase db)
        Left _ -> (toSimpleDatabase db, ["The inputs supplied by other databases are written as their source named them: not all their suppliers could be worked out." | not (null (dbCrossDBLinks db))])
    -- These writers write no parameter: what they write as values is said.
    valuesOnlyWith own = fmap (,own <> valuesOnly sdb)

{- | Each input a dependency supplies, written under that supplier's product
and identity rather than under the name its source gave it.

A relink can answer « market group for electricity, medium voltage » {GLO} with
« Electricity, medium voltage, at grid » {RER} in another database. Written as
the source said it, the line reads back beside that database asking for a
product it does not ship, and links to nothing. So the line takes the
supplier's product, and points at the supplier, for the formats that write an
identifier; the formats that name a supplier read its activity and location
from the dependencies themselves.

The amount stays as written when its unit converts to the supplier's, since a
reader converts it the same way. A link that bridged two measures (a relink
mapping row with a conversion) is written in the link's unit, its amount
converted, and its formula, which spoke of the other measure, dropped and said.
The line takes the sign that makes the format, reading it against the
supplier, ask what the link asks. A line reaching a treatment written as giving
out its waste changes sign when the format reads such a line the other way than
the database does: SimaPro's +3 kg sent to it is EcoSpold 2's -3 kg. A line a
relink mapping turned (its @direction@ column) is written turned. A turned
line's formula is dropped and said too.
-}
namedAfterLinks :: UnitConfig -> ReferenceConvention -> Dependencies -> [CrossDBLink] -> SimpleDatabase -> (SimpleDatabase, [Text])
namedAfterLinks units target (Dependencies deps) links sdb0 = foldl' answer (sdb0, []) links
  where
    answer :: (SimpleDatabase, [Text]) -> CrossDBLink -> (SimpleDatabase, [Text])
    answer (sdb, said) link = fromMaybe (sdb, said) $ do
        AnsweredExchange{aePosition = position, aeFactor = factor} <- cdlAnswers link
        let key = (cdlConsumerActUUID link, cdlConsumerProdUUID link)
        supplier <- M.lookup (fst key, snd key, position) deps
        activity <- M.lookup key (sdbActivities sdb)
        (before, line@TechnosphereExchange{techFlowId = flow, techAmount = amount, techUnitId = unit, techFormula = formula} : after) <- Just (splitAt position (exchanges activity))
        consumerFlow <- if flow == cdlConsumerFlowId link then M.lookup flow (sdbTechFlows sdb) else Nothing
        let bridged = isNothing (lineInLinkUnit units (sdbUnits sdb) line (cdlExchangeUnit link))
            -- The link asks factor × amount; the format reads the written line times its own sign.
            turned = signum factor * readingSign target (elReferenceSign supplier)
            linkUnit = fromMaybe (newUnit (cdlExchangeUnit link)) (find ((== cdlExchangeUnit link) . unitName) (M.elems (sdbUnits sdb)))
            -- The consumer's own unit: the supplier's lives in another database's table.
            supplied =
                consumerFlow
                    { tfId = cdlSupplierProdUUID link
                    , tfName = cdlFlowName link
                    , tfUnitId = if bridged then unitId linkUnit else tfUnitId consumerFlow
                    , tfSynonyms = M.empty
                    }
            written =
                line
                    { techFlowId = tfId supplied
                    , techActivityLinkId = Just (cdlSupplierActUUID link)
                    , -- The factor's sign is how the database reads the line, not part of the conversion.
                      techAmount = (if bridged then amount * abs factor else amount) * turned
                    , techUnitId = if bridged then unitId linkUnit else unit
                    , techFormula = if bridged || turned < 0 then Nothing else formula
                    }
            formulaDropped =
                [ activityName activity <> ", input " <> tfName consumerFlow <> ": written as a value in " <> cdlExchangeUnit link <> ", its formula spoke of another measure"
                | bridged
                , Just _ <- [formula]
                ]
                    <> [ activityName activity <> ", input " <> tfName consumerFlow <> ": written as a value of the opposite sign, the sign the format reads against its supplier"
                       | not bridged
                       , turned < 0
                       , Just _ <- [formula]
                       ]
        pure
            ( sdb
                { sdbActivities = M.insert key activity{exchanges = before <> (written : after)} (sdbActivities sdb)
                , sdbTechFlows = M.insertWith (\_ kept -> kept) (tfId supplied) supplied (sdbTechFlows sdb)
                , sdbUnits = if bridged then M.insertWith (\_ kept -> kept) (unitId linkUnit) linkUnit (sdbUnits sdb) else sdbUnits sdb
                }
            , said <> formulaDropped
            )

    newUnit :: Text -> Unit
    newUnit name = Unit{unitId = generateUnitUUID name, unitName = name, unitSymbol = name, unitComment = ""}

{- | Export targets for a method collection – a space of its own, not
'DatabaseFormat': most database formats carry no method writer, and method
formats need not be database formats at all. A request naming a format
outside this type is rejected at parse time instead of dispatching into a
wall of runtime 'Left's.
-}
data MethodExportFormat
    = MethodSimaProCSV -- SimaPro method CSV ({methods} block)
    | MethodColumnarCSV -- columnar CSV (one column per impact category)
    | MethodOpenLcaJsonLd -- openLCA JSON-LD zip (one ImpactCategory per method)
    | MethodIlcdXml -- ILCD LCIA-method package zip (lciamethods/ + flows/)
    deriving (Show, Eq)

{- | Parse a user-facing method-export format name (case- and
whitespace-insensitive). Shared by the CLI and the HTTP handler so the
accepted spellings and the error message stay in one place.
-}
parseMethodExportFormat :: Text -> Either Text MethodExportFormat
parseMethodExportFormat raw = case T.toLower (T.strip raw) of
    "simapro" -> Right MethodSimaProCSV
    "csv" -> Right MethodColumnarCSV
    "openlca" -> Right MethodOpenLcaJsonLd
    "ilcd" -> Right MethodIlcdXml
    other -> Left ("unknown method export format: " <> other <> " (expected simapro|csv|openlca|ilcd)")

{- | Serialize a loaded method collection in the requested format, paired with
the projection warnings. The name is the collection's own (used as the
file-level method name when the impact categories don't share a methodology).
-}
serializeMethodCollection :: MethodExportFormat -> Text -> MethodCollection -> Either Text (BL.ByteString, [Text])
serializeMethodCollection fmt name mc = case fmt of
    MethodSimaProCSV ->
        first BL.fromStrict
            <$> MW.serializeSimaProMethodCSV SP.defaultWriterConfig name (withoutExclusions mc)
    MethodColumnarCSV ->
        first BL.fromStrict <$> MWC.serializeColumnarMethodCSV mc
    MethodOpenLcaJsonLd ->
        first zipFiles <$> MWO.serializeOlcaMethodEntries (withoutExclusions mc)
    MethodIlcdXml ->
        first zipFiles <$> MWI.serializeIlcdMethodEntries (withoutExclusions mc)

{- | Drop the exclusion rows before writing a format that has no notion of one.
A @"!Occupation, sea*"@ row would otherwise land as a flow characterized at the
number its value cell only ever used to name a category – the export would
characterize exactly what the exception takes out. The columnar CSV is VoLCA's
own method format and reads the marker back, so it keeps them and round-trips.
-}
withoutExclusions :: MethodCollection -> MethodCollection
withoutExclusions mc = mc{mcMethods = map dropExclusionRows (mcMethods mc)}
  where
    dropExclusionRows m = m{methodFactors = filter (not . isExclusionCF) (methodFactors m)}

{- | Parse a user-facing export-format name (case- and whitespace-insensitive)
to a 'DatabaseFormat'. Shared by the CLI and the HTTP handler so the accepted
spellings and the error message stay in one place.
-}
parseExportFormat :: Text -> Either Text DatabaseFormat
parseExportFormat raw = case T.toLower (T.strip raw) of
    "simapro" -> Right SimaProCSV
    "ecospold1" -> Right EcoSpold1
    "ecospold2" -> Right EcoSpold2
    "ilcd" -> Right ILCDProcess
    "brightway" -> Right BrightwayExcel
    "openlca" -> Right OpenLcaPackage
    other -> Left ("unknown export format: " <> other <> " (expected simapro|ecospold1|ecospold2|ilcd|brightway|openlca)")

-- | File variant of 'serializeMethodCollection', for the CLI.
exportMethodCollection :: MethodExportFormat -> Text -> MethodCollection -> FilePath -> IO (Either Text [Text])
exportMethodCollection fmt name mc path = writeExport path (serializeMethodCollection fmt name mc)

-- | Write serialized bytes to @path@, passing the warnings through.
writeExport :: FilePath -> Either Text (BL.ByteString, [Text]) -> IO (Either Text [Text])
writeExport path serialized = case serialized of
    Left err -> pure (Left err)
    Right (bytes, warnings) -> either (Left . renderErr) (const (Right warnings)) <$> try (BL.writeFile path bytes)
  where
    renderErr :: SomeException -> Text
    renderErr e = "export failed: " <> T.pack (show e)

-- | Pack @(path, text)@ entries into a deterministic zip.
zipText :: [(FilePath, Text)] -> BL.ByteString
zipText = zipFiles . map (fmap TE.encodeUtf8)
