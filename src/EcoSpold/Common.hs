{-# LANGUAGE OverloadedStrings #-}

-- | Common utilities shared between EcoSpold1 and EcoSpold2 parsers
module EcoSpold.Common (
    bsToText,
    decodeXmlEntities,
    bsToDouble,
    bsToInt,
    isElement,
    distributeFiles,
) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR

-- | ByteString to Text conversion with UTF-8 decoding and XML entity decoding
bsToText :: BS.ByteString -> Text
bsToText = decodeXmlEntities . TE.decodeUtf8

{- | Decode common XML entities that Xeno doesn't decode
Xeno is a fast SAX parser but doesn't handle entity references
-}
decodeXmlEntities :: Text -> Text
decodeXmlEntities =
    T.replace "&lt;" "<"
        . T.replace "&gt;" ">"
        . T.replace "&amp;" "&"
        . T.replace "&quot;" "\""
        . T.replace "&apos;" "'"

-- | ByteString to Double conversion (strict - errors on parse failure)
bsToDouble :: BS.ByteString -> Double
bsToDouble bs = case TR.double (bsToText bs) of
    Right (val, _) -> val
    Left _ -> error $ "Failed to parse double from: " ++ show bs

-- | ByteString to Int conversion (strict - errors on parse failure)
bsToInt :: BS.ByteString -> Int
bsToInt bs = case TR.decimal (bsToText bs) of
    Right (val, _) -> val
    Left _ -> error $ "Failed to parse int from: " ++ show bs

{- | Check if element name matches (with or without namespace prefix)
Handles both "tagName" and "prefix:tagName" forms
-}
isElement :: BS.ByteString -> BS.ByteString -> Bool
isElement tagName expected =
    tagName == expected || BS.isSuffixOf (":" `BS.append` expected) tagName

-- | Distribute a list evenly across N buckets (for parallel workers)
distributeFiles :: Int -> [a] -> [[a]]
distributeFiles n xs =
    let len = length xs
        baseSize = len `div` n
        remainder = len `mod` n
        sizes = replicate remainder (baseSize + 1) ++ replicate (n - remainder) baseSize
     in go sizes xs
  where
    go [] _ = []
    go _ [] = []
    go (s : ss) ys = let (h, t) = splitAt s ys in h : go ss t
