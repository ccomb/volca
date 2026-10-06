{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- | What the engine was used for, kept for whoever runs it to collect.

Each computation on a process of a database that declares its release leaves
one line: which kind of computation, which process and what the database
calls it, which releases it read, and the reader the request named, if any. A database without a declared
release leaves nothing, since nothing would say whose data was read.

The lines are held in memory, not in a file: a collector reads them as they
come and then tells the engine to forget what it kept, so a file would only
outlive a crash, and a crash then loses what came since the last collection.
Each start draws a new 'BootId', so a collector that finds another one knows
the numbering started again and reads from the beginning.
-}
module Usage (
    UsageKind (..),
    usageKindCode,
    BootId (..),
    ProcessKey (..),
    ProcessNaming (..),
    Use (..),
    Lookup (..),
    UsageLine (..),
    UsagePage (..),
    UsageLog,
    newUsageLog,
    usageBoot,
    recordUse,
    readerHeader,
    readerOf,
    readUsage,
    forgetUsage,
    LogState (..),
    emptyLog,
    appendLine,
    pageAfter,
    forgetThrough,
    usageCap,
    usagePageSize,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, stateTVar)
import Control.Monad (forM_, when)
import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.OpenApi (ToParamSchema, ToSchema (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import Network.HTTP.Types (HeaderName, RequestHeaders)
import Servant (FromHttpApiData, ToHttpApiData)

import API.JsonOptions (Stripped (..))
import Progress (ProgressLevel (..), reportProgress)
import Types (Release, codeSchema, parseCode)

-- | What a computation did with the process it was asked about.
data UsageKind
    = -- | Its exchanges, its supply chain, the processes around it
      Reading
    | -- | Its life-cycle inventory, or a part of it summed
      Inventorying
    | -- | Its impact scores
      Scoring
    | -- | What weighs in one of its scores
      Contributing
    | -- | Its scores or its inventory set beside another process's
      Comparing
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

usageKindCode :: UsageKind -> Text
usageKindCode Reading = "reading"
usageKindCode Inventorying = "inventory"
usageKindCode Scoring = "scoring"
usageKindCode Contributing = "contributions"
usageKindCode Comparing = "comparison"

instance ToJSON UsageKind where
    toJSON = toJSON . usageKindCode

instance FromJSON UsageKind where
    parseJSON = withText "UsageKind" (either (fail . T.unpack) pure . parseCode "usage kind" usageKindCode)

instance ToSchema UsageKind where
    declareNamedSchema _ = pure (codeSchema "UsageKind" usageKindCode)

-- | One start of the engine; the numbering of its lines begins again with each.
newtype BootId = BootId Text
    deriving (Show, Eq, Generic)
    deriving newtype (ToJSON, FromJSON, ToSchema, FromHttpApiData, ToHttpApiData, ToParamSchema)

{- | A process as the request named it, kept apart from the database name it
always travels beside so that the two cannot be swapped.
-}
newtype ProcessKey = ProcessKey Text
    deriving (Show, Eq, Generic)
    deriving newtype (ToJSON, FromJSON, ToSchema)

-- | A computation as the surface that ran it describes it, before its releases and what the database calls the process are looked up.
data Use = Use
    { useKind :: !UsageKind
    , useDatabase :: !Text
    , useProcess :: !ProcessKey
    }
    deriving (Show, Eq)

{- | What a database calls a process: its activity, its product and its
location, kept apart as everywhere else on the wire. A process has no name of
its own; whoever shows one composes it from these.
-}
data ProcessNaming = ProcessNaming
    { pnActivityName :: !Text
    , pnProductName :: !(Maybe Text)
    -- ^ None when the activity has no reference exchange, or one naming a flow the database does not hold
    , pnLocation :: !Text
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped ProcessNaming)

-- | One computation, as a collector reads it.
data UsageLine = UsageLine
    { ulSeq :: !Int
    , ulAt :: !UTCTime
    , ulKind :: !UsageKind
    , ulDatabase :: !Text
    , ulProcess :: !ProcessKey
    , ulHeldAs :: !(Maybe ProcessNaming)
    -- ^ What the database calls the process; none when the database no longer holds it
    , ulReader :: !(Maybe Text)
    -- ^ Whoever the request said it was made for, as the server in front of the engine named them
    , ulReads :: !(NonEmpty Release)
    -- ^ The database's own release when it has one, then those of the databases it depends on
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped UsageLine)

-- | The lines after a collector's cursor, at most 'usagePageSize' of them.
data UsagePage = UsagePage
    { upBoot :: !BootId
    , upLines :: ![UsageLine]
    , upMore :: !Bool
    -- ^ More lines wait after these
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, FromJSON, ToSchema) via (Stripped UsagePage)

-- | The lines kept, and the number the next one takes.
data LogState = LogState
    { lsNext :: !Int
    , lsLines :: !(Seq UsageLine)
    }
    deriving (Show, Eq)

data UsageLog = UsageLog
    { logBoot :: !BootId
    , logState :: !(TVar LogState)
    }

{- | How many lines are kept when nobody collects them. Past it the oldest go,
and the engine says how many: a collector that stopped coming is a fault to
see, not a reason to grow without end.
-}
usageCap :: Int
usageCap = 100000

-- | How many lines one read returns.
usagePageSize :: Int
usagePageSize = 1000

emptyLog :: LogState
emptyLog = LogState{lsNext = 1, lsLines = Seq.empty}

newUsageLog :: IO UsageLog
newUsageLog = UsageLog <$> fmap (BootId . UUID.toText) UUID.nextRandom <*> newTVarIO emptyLog

usageBoot :: UsageLog -> BootId
usageBoot = logBoot

{- | Keep one line, numbered, and the number of the oldest ones dropped to stay
within the cap.
-}
appendLine :: Int -> (Int -> UsageLine) -> LogState -> (Int, LogState)
appendLine cap mkLine (LogState next kept) =
    let grown = kept Seq.|> mkLine next
        dropped = max 0 (Seq.length grown - cap)
     in (dropped, LogState (next + 1) (Seq.drop dropped grown))

-- | The lines numbered after @cursor@, and whether more follow the page.
pageAfter :: Int -> Int -> LogState -> ([UsageLine], Bool)
pageAfter size cursor st =
    let after = Seq.dropWhileL ((<= cursor) . ulSeq) (lsLines st)
     in (F.toList (Seq.take size after), Seq.length after > size)

-- | Forget the lines numbered up to @through@: the collector has kept them.
forgetThrough :: Int -> LogState -> LogState
forgetThrough through st = st{lsLines = Seq.dropWhileL ((<= through) . ulSeq) (lsLines st)}

{- | The request header naming whoever a request was made for. The engine
copies it into the line as it is: the server in front of it decides what names
a reader, and is the one that must strip it from what callers send.
-}
readerHeader :: HeaderName
readerHeader = "Volca-Reader"

readerOf :: RequestHeaders -> Maybe Text
readerOf = fmap TE.decodeUtf8Lenient . lookup readerHeader

-- | What the engine knows of the database and the process a computation ran on.
data Lookup = Lookup
    { releasesOf :: Text -> IO [Release]
    -- ^ The releases a database reads: its own, then those of the databases it depends on
    , namingOf :: Text -> ProcessKey -> IO (Maybe ProcessNaming)
    }

{- | Keep a line for a computation, when the database it ran on, or one it
depends on, declares a release.
-}
recordUse :: UsageLog -> Lookup -> Maybe Text -> Use -> IO ()
recordUse lg known reader use = do
    releases <- releasesOf known (useDatabase use)
    forM_ (nonEmpty releases) $ \readReleases -> do
        naming <- namingOf known (useDatabase use) (useProcess use)
        now <- getCurrentTime
        recordLine lg (\n -> UsageLine n now (useKind use) (useDatabase use) (useProcess use) naming reader readReleases)

recordLine :: UsageLog -> (Int -> UsageLine) -> IO ()
recordLine lg mkLine = do
    dropped <- atomically (stateTVar (logState lg) (appendLine usageCap mkLine))
    when (dropped > 0) $
        reportProgress Warning $
            "Usage log full: dropped the "
                <> show dropped
                <> " oldest line(s), nobody collected them"

readUsage :: UsageLog -> Int -> IO UsagePage
readUsage lg cursor = do
    (ls, more) <- pageAfter usagePageSize cursor <$> readTVarIO (logState lg)
    pure UsagePage{upBoot = logBoot lg, upLines = ls, upMore = more}

{- | Forget what a collector kept. Refused when it read another start's lines:
a cursor from an earlier start says nothing about these, which it has not seen.
-}
forgetUsage :: UsageLog -> BootId -> Int -> IO (Either Text ())
forgetUsage lg boot through
    | boot /= logBoot lg = pure (Left "These lines are from another start of the engine; read them first")
    | otherwise = Right <$> atomically (modifyTVar' (logState lg) (forgetThrough through))
