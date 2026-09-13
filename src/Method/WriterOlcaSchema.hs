{-# LANGUAGE OverloadedStrings #-}

{- | Writer for LCIA methods as openLCA JSON-LD – the exact inverse of
'Method.Parser.OlcaSchema'. Each method becomes one @ImpactCategory@
document, named @lcia_categories/\<method-uuid\>.json@ (the olca-schema
archive layout, which the loader's directory scan finds again on
re-import). The caller packs the entries into a zip.

What round-trips exactly: the method UUID, name, category label,
description, reference unit, and per-factor flow UUID/name/CAS, value,
unit, direction (@INPUT@/@OUTPUT@ is native here – no compartment
heuristic), and location code. Entries are sorted by file name so a
loaded-then-re-exported archive is byte-identical.

Projection (documented, no warning): a compartment qualifier folds into
the subcompartment – the category path @medium/sub/qualifier@ reads back
as @(medium, \"sub\/qualifier\")@, the same fold the SimaPro writer applies
to long-term.

Not representable (reported as warnings): methodology labels (re-import
stamps \"openLCA JSON-LD\"), blank category labels (they read back as the
name), damage categories, normalization\/weighting sets, and scoring sets.
-}
module Method.WriterOlcaSchema (
    serializeOlcaMethodEntries,
    checkOlcaExportable,
) where

import Data.Aeson (KeyValue ((.=)))
import Data.Aeson.Encoding (Encoding, Series, encodingToLazyByteString, list, pair, pairs)
import qualified Data.Aeson.Key as K
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Indexing (collisions)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID

import Method.Types

{- | Serialize a method collection to openLCA JSON-LD zip entries, paired
with the projection warnings. 'Left' when the format cannot represent the
collection without silent corruption on re-import ('checkOlcaExportable').
-}
serializeOlcaMethodEntries :: MethodCollection -> Either Text ([(FilePath, BS.ByteString)], [Text])
serializeOlcaMethodEntries mc = do
    checkOlcaExportable mc
    let entries =
            [ (entryPath m, BL.toStrict (encodingToLazyByteString (methodDoc m)))
            | m <- mcMethods mc
            ]
    pure (sortOn fst entries, lossWarnings mc)

-- | Archive entry for one method, keyed by its UUID (unique by the guard).
entryPath :: Method -> FilePath
entryPath m = "lcia_categories/" <> UUID.toString (methodId m) <> ".json"

{- | One @ImpactCategory@ document. Keys the parser reads nothing from when
empty (description, unit, CAS, …) are omitted rather than emitted blank –
the parser treats an empty string as absent, so both spellings read back
the same and only one of them is canonical.
-}
methodDoc :: Method -> Encoding
methodDoc m =
    pairs $
        "@context" .= ("http://greendelta.github.io/olca-schema/context.jsonld" :: Text)
            <> "@type" .= ("ImpactCategory" :: Text)
            <> "@id" .= UUID.toText (methodId m)
            <> "name" .= methodName m
            <> categorySeries
            <> maybe mempty (optKey "description") (methodDescription m)
            <> optKey "referenceUnitName" (methodUnit m)
            <> pair "impactFactors" (list factorDoc (methodFactors m))
  where
    -- The parser falls back to the name when category is absent, so a label
    -- equal to the name needs no field of its own.
    categorySeries
        | methodCategory m == methodName m = mempty
        | otherwise = optKey "category" (methodCategory m)

factorDoc :: MethodCF -> Encoding
factorDoc cf =
    pairs $
        "@type" .= ("ImpactFactor" :: Text)
            <> "value" .= mcfValue cf
            <> pair "flow" (flowDoc cf)
            <> "direction" .= directionText (mcfDirection cf)
            <> optRef "unit" "Unit" "name" (mcfUnit cf)
            <> maybe mempty (optRef "location" "Location" "code") (mcfConsumerLocation cf)

flowDoc :: MethodCF -> Encoding
flowDoc cf =
    pairs $
        "@type" .= ("Flow" :: Text)
            <> "@id" .= UUID.toText (mcfFlowRef cf)
            <> "name" .= mcfFlowName cf
            <> "flowType" .= ("ELEMENTARY_FLOW" :: Text)
            <> maybe mempty (optKey "cas") (mcfCAS cf)
            <> maybe mempty categoryRef (mcfCompartment cf)

-- | The compartment as a @Category@ Ref whose name is the slash path.
categoryRef :: Compartment -> Series
categoryRef comp =
    pair "category" (pairs ("@type" .= ("Category" :: Text) <> "name" .= compartmentPath comp))

{- | The @medium/sub/qualifier@ path, segments stripped (the parser strips
what it reads back), empty segments dropped.
-}
compartmentPath :: Compartment -> Text
compartmentPath (Compartment medium sub qualifier) =
    T.intercalate "/" (filter (not . T.null) (map T.strip [medium, sub, qualifier]))

directionText :: FlowDirection -> Text
directionText Input = "INPUT"
directionText Output = "OUTPUT"

-- | Emit @key@ only when the value is non-empty.
optKey :: Text -> Text -> Series
optKey key value
    | T.null value = mempty
    | otherwise = K.fromText key .= value

-- | A one-field Ref object (@unit@, @location@), omitted when the value is empty.
optRef :: Text -> Text -> Text -> Text -> Series
optRef key refType field value
    | T.null value = mempty
    | otherwise = pair (K.fromText key) (pairs ("@type" .= refType <> K.fromText field .= value))

{- | Representation losses the format cannot carry; counted, never dropped
silently. Blank category labels are a loss (not a guard) because the
collection still re-imports correctly except for that one label.
-}
lossWarnings :: MethodCollection -> [Text]
lossWarnings mc =
    mapMaybe
        (\(count, what) -> if count == 0 then Nothing else Just (T.pack (show count) <> " " <> what))
        [ (S.size lostMethodologies, "methodology labels are not representable in openLCA JSON-LD (re-import reads \"openLCA JSON-LD\")")
        , (length blankCategories, "blank impact category group labels read back as the category name")
        , (length (mcDamageCategories mc), "damage categories are not representable in openLCA JSON-LD")
        , (length (mcNormWeightSets mc), "normalization/weighting sets are not representable in openLCA JSON-LD")
        , (length (mcScoringSets mc), "formula scoring sets are not representable in openLCA JSON-LD")
        ]
  where
    -- "openLCA JSON-LD" is what re-import stamps, so that label survives.
    lostMethodologies =
        S.fromList [x | Just x <- map methodMethodology (mcMethods mc), x /= "openLCA JSON-LD"]
    blankCategories =
        [() | m <- mcMethods mc, T.null (methodCategory m), not (T.null (methodName m))]

{- | Reject a collection the format cannot represent without silent
corruption on re-import: no impact categories, two methods sharing a UUID
(their archive entries would overwrite each other), a factor with no flow
name (the parser silently drops it), a non-finite value (JSON encodes it
as @null@ and the parser silently drops the factor), or a compartment
whose medium is empty or contains @/@ (the path would read back shifted
or compartment-less).
-}
checkOlcaExportable :: MethodCollection -> Either Text ()
checkOlcaExportable mc
    | null (mcMethods mc) = Left "method collection has no impact categories"
    | otherwise = do
        checkUniqueIds (mcMethods mc)
        mapM_ checkMethod (mcMethods mc)
  where
    checkUniqueIds :: [Method] -> Either Text ()
    checkUniqueIds ms =
        case collisions [(methodId m, methodName m) | m <- ms] of
            [] -> Right ()
            (mid, names) : _ ->
                Left
                    ( "impact categories share the id "
                        <> UUID.toText mid
                        <> " ("
                        <> T.intercalate ", " (NE.toList names)
                        <> "); their archive entries would overwrite each other"
                    )
    checkMethod m = mapM_ (checkCF (methodName m)) (methodFactors m)
    checkCF cat cf = do
        finite ("characterization factor for '" <> mcfFlowName cf <> "' in '" <> cat <> "'") (mcfValue cf)
        checkFlowName cat cf
        mapM_ checkCompartment (mcfCompartment cf)
    checkFlowName cat cf
        | T.null (mcfFlowName cf) =
            Left ("a characterization factor in '" <> cat <> "' has no flow name; re-import would silently drop it")
        | otherwise = Right ()
    checkCompartment (Compartment medium _ _)
        | T.null (T.strip medium) = Left "compartment has an empty medium"
        | T.any (== '/') medium =
            Left ("compartment medium contains '/' (the path separator): " <> T.take 60 medium)
        | otherwise = Right ()
    finite label v
        | isNaN v || isInfinite v = Left ("non-finite " <> label)
        | otherwise = Right ()
