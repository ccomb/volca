{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- | Format dispatch for database export.

Inverts the upload/parse path: given an in-memory 'Database', serialize it to
bytes in one of the supported formats by delegating to the per-format writer.

Single-file formats (SimaPro CSV, EcoSpold 1, Brightway Excel) serialize to one
byte stream. Multi-file formats (EcoSpold 2, ILCD) are inherently directory
trees, so they are packaged into a deterministic zip archive. 'OpenLcaJsonLd'
has no writer and 'UnknownFormat' is not a real target, so both fail loudly
('Left') rather than emit a silent empty file.
-}
module Database.Export (
    serializeDatabase,
    exportDatabase,
    MethodExportFormat (..),
    parseMethodExportFormat,
    serializeMethodCollection,
    exportMethodCollection,
    parseExportFormat,
) where

import Control.Exception (SomeException, try)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified BrightwayExcel.Writer as BE
import Database.Upload (DatabaseFormat (..))
import qualified EcoSpold.Writer1 as ES1
import qualified EcoSpold.Writer2 as ES2
import qualified ILCD.Writer as ILCD
import Method.Mapping (isExclusionCF)
import Method.Types (Method (..), MethodCollection (..))
import qualified Method.WriterCSV as MWC
import qualified Method.WriterILCD as MWI
import qualified Method.WriterOlcaSchema as MWO
import qualified Method.WriterSimaPro as MW
import qualified SimaPro.Writer as SP
import Types (Database, toSimpleDatabase)
import Zip (zipFiles)

{- | Serialize a database to a single byte stream in the requested format, paired
with any best-effort approximation warnings. Pure: the multi-file formats are
zipped in-memory. Fails loudly for formats without a writer.

The warning list is empty for a faithful export and non-empty when a writer had
to approximate. Two writers approximate today: Brightway has no waste type, so
it rewrites /orphan/ waste exchanges as technosphere flows (inventory-neutral,
but the waste tag is lost on re-import); ILCD keys one process per dataset UUID,
so a multi-output activity's products export as separate, unlinked datasets
('ILCD.Writer.splitWarnings'). Returning bytes and warnings together shares the
one 'toSimpleDatabase' conversion and keeps them from drifting apart.
-}
serializeDatabase :: DatabaseFormat -> Database -> Either Text (BL.ByteString, [Text])
serializeDatabase fmt db = case fmt of
    -- Each writer runs its own check*Exportable and returns 'Left' on a database
    -- the format cannot represent faithfully, so the guard is unskippable.
    SimaProCSV -> noWarn (BL.fromStrict <$> SP.serializeSimaProCSV SP.defaultWriterConfig sdb)
    EcoSpold1 -> noWarn (BL.fromStrict . TE.encodeUtf8 <$> ES1.writeDatabase ES1.canonicalWriterOptions db)
    EcoSpold2 -> noWarn (zipText <$> ES2.writeEcoSpold2 ES2.noVolatileMeta sdb)
    ILCDProcess -> (,ILCD.splitWarnings sdb) <$> ILCD.writeILCDArchive ILCD.defaultWriteOptions sdb
    BrightwayExcel -> (,BE.wasteManifest sdb) <$> BE.renderWorkbook BE.defaultWriterConfig sdb
    OpenLcaJsonLd ->
        Left "openLCA JSON-LD export is not supported"
    UnknownFormat ->
        Left "cannot export to an unknown format"
  where
    sdb = toSimpleDatabase db
    noWarn = fmap (,[])

{- | Export targets for a method collection - a space of its own, not
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
number its value cell only ever used to name a category - the export would
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
    other -> Left ("unknown export format: " <> other <> " (expected simapro|ecospold1|ecospold2|ilcd|brightway)")

{- | Serialize a database and write it to @path@, returning the approximation
warnings so the caller can report them - a local export approximates exactly as
much as a remote one.
-}
exportDatabase :: DatabaseFormat -> Database -> FilePath -> IO (Either Text [Text])
exportDatabase fmt db path = writeExport path (serializeDatabase fmt db)

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
