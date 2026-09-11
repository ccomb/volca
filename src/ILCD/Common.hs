{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | What an ILCD file declares about itself, and what a directory of them adds
up to.

An ILCD package holds one XML file per dataset, and a dataset is named by the
UUID inside the file, not by the file's name. Two files can therefore claim one
UUID - which is how the format publishes a revision, the superseded file often
staying beside the new one - and a directory listing says nothing about which
is which. The version each file declares does, and that is what 'latestByUUID'
reads.
-}
module ILCD.Common (
    listXMLFiles,
    DataSetVersion,
    readDataSetVersion,
    showDataSetVersion,
    declaredVersion,
    Claimed (..),
    Indexed (..),
    latestByUUID,
) where

import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.List (sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import EcoSpold.Common (bsToText, isElement)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import qualified Xeno.SAX as X

{- | The XML files of a directory, in one order.

Sorted, because 'listDirectory' answers in whatever order the file system holds
them: an unsorted listing makes a database depend on the machine it was read on
before anything else has a chance to.
-}
listXMLFiles :: FilePath -> IO [FilePath]
listXMLFiles d = do
    fs <- listDirectory d
    return (sort [d </> f | f <- fs, map toLower (takeExtension f) == ".xml"])

{- | The version an ILCD dataset declares, as the numbers it is compared on.

@\<common:dataSetVersion\>@ is written @03.00.000@, three groups counting major,
minor and patch. The comparison is on the numbers and not on the text, where
@10.00.000@ would sort below @09.00.000@.
-}
newtype DataSetVersion = DataSetVersion (NonEmpty Int)
    deriving (Eq, Ord, Show)

-- | Read a declared version, or nothing when it is not a run of dotted numbers.
readDataSetVersion :: Text -> Maybe DataSetVersion
readDataSetVersion t = DataSetVersion <$> (NE.nonEmpty =<< traverse group (T.splitOn "." (T.strip t)))
  where
    group :: Text -> Maybe Int
    group g = case TR.decimal g of
        Right (n, rest) | T.null rest -> Just n
        _ -> Nothing

-- | The version as a message names it: the numbers, without the source padding.
showDataSetVersion :: DataSetVersion -> Text
showDataSetVersion (DataSetVersion ns) = T.intercalate "." (map (T.pack . show) (NE.toList ns))

{- | The version an ILCD file declares, read on its own.

Every kind of ILCD dataset carries @\<common:dataSetVersion\>@ in the same place
and nothing else does, so one reader serves the four the engine opens and none
of their parsers has to grow a field for a value they only need when two files
collide. The first one wins, since a file that names another dataset names it
by reference and not by repeating the element.
-}
declaredVersion :: BS.ByteString -> Maybe DataSetVersion
declaredVersion bytes = case X.fold openTag attr endOpen txt closeTag cdata (VState Nothing []) bytes of
    Left _ -> Nothing
    Right s -> vsVersion s
  where
    openTag s _ = s{vsAccum = []}
    attr s _ _ = s
    endOpen s _ = s
    txt s content = s{vsAccum = content : vsAccum s}
    cdata = txt
    closeTag s tag
        | isElement tag "dataSetVersion" && isNothing (vsVersion s) =
            s{vsVersion = readDataSetVersion (T.concat (reverse (map bsToText (vsAccum s)))), vsAccum = []}
        | otherwise = s{vsAccum = []}

data VState = VState
    { vsVersion :: !(Maybe DataSetVersion)
    , vsAccum :: ![BS.ByteString]
    }

-- | One file, the UUID it claimed, and what was read out of it.
data Claimed a = Claimed
    { claimFile :: !FilePath
    , claimUUID :: !UUID
    , claimContent :: !a
    }

{- | A directory read into one dataset per UUID, and the files that lost.

'ixSuperseded' carries one line per file a newer version replaced, for the
caller to report: a file dropped without a word is a dataset the user believes
they loaded.
-}
data Indexed a = Indexed
    { ixByUUID :: !(M.Map UUID a)
    , ixSuperseded :: ![Text]
    }

{- | One dataset per UUID: the file declaring the highest version, and a word
about each file it replaces.

Two files at the same highest version name no winner, and neither do two that
both leave the version out. Either way the read stops instead of keeping
whichever the listing reached first, because the answer is supposed to be in
the files and a listing would decide it by file name.

Only the files of a UUID that two of them claim are read again for their
version: where every UUID appears once there is nothing to arbitrate, which is
every directory that is in order.
-}
latestByUUID :: forall a. [Claimed a] -> IO (Either Text (Indexed a))
latestByUUID claimed = decided <$> traverse rank grouped
  where
    grouped :: [(UUID, NonEmpty (Claimed a))]
    grouped = M.toList (M.fromListWith (<>) [(claimUUID c, c :| []) | c <- claimed])

    -- The claims on one UUID, highest version first. One claim answers alone.
    rank :: (UUID, NonEmpty (Claimed a)) -> IO (NonEmpty (Declared a))
    rank (_, only :| []) = return (Declared only Nothing :| [])
    rank (_, claims) = NE.sortBy (comparing (Down . declVersion)) <$> traverse declaring claims

    declaring :: Claimed a -> IO (Declared a)
    declaring c = Declared c . declaredVersion <$> BS.readFile (claimFile c)

    decided :: [NonEmpty (Declared a)] -> Either Text (Indexed a)
    decided ranked = case concatMap tied ranked of
        (t : ts) -> Left (refusal (t :| ts))
        [] ->
            Right
                Indexed
                    { ixByUUID = M.fromList [(claimUUID (declClaim best), claimContent (declClaim best)) | best :| _ <- ranked]
                    , ixSuperseded = concatMap superseded ranked
                    }

    -- The files sharing the highest version, when more than one does.
    tied :: NonEmpty (Declared a) -> [NonEmpty FilePath]
    tied (best :| rest) = case filter ((== declVersion best) . declVersion) rest of
        [] -> []
        others -> [NE.sort (fileOf best :| map fileOf others)]

    superseded :: NonEmpty (Declared a) -> [Text]
    superseded (best :| rest) =
        [ T.pack (fileOf beaten)
            <> " is superseded: "
            <> UUID.toText (claimUUID (declClaim best))
            <> " is also declared by "
            <> T.pack (fileOf best)
            <> maybe "" (\v -> ", at version " <> showDataSetVersion v) (declVersion best)
        | beaten <- sortOn fileOf rest
        ]

    fileOf :: Declared a -> FilePath
    fileOf = claimFile . declClaim

    refusal :: NonEmpty (NonEmpty FilePath) -> Text
    refusal clashes =
        "two files declare one dataset at the same version, and nothing says which is meant: "
            <> T.intercalate "; " (map named (NE.toList clashes))

    named :: NonEmpty FilePath -> Text
    named files = T.intercalate ", " (map T.pack (NE.toList files))

-- | A claim, once the version behind it had to be read.
data Declared a = Declared
    { declClaim :: !(Claimed a)
    , declVersion :: !(Maybe DataSetVersion)
    }
