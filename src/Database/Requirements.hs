{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The releases a packaged database was built on, and which of this engine's
databases may stand for them. A database that requires none links to every
database loaded, as it always has; one that requires some links only to a
database of a required release, or to the one its reader accepted in that
release's place. A model is not computed on other data than its author's
unless its reader says so.
-}
module Database.Requirements (
    admits,
    requiredDatabases,
    Satisfaction (..),
    RequiredRelease (..),
    requiredReleases,
    Substitution (..),
    acceptSubstitute,
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Map.Strict as M
import Data.OpenApi (ToSchema (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import API.JsonOptions (Stripped (..), parseClosed)

import Config (DatabaseConfig (..))
import Types (Release (..), Requirement (..), codeSchema, sameRelease, sameReleaseName)

-- | Whether a database may meet one requirement: it is the one accepted in its place, or, with none accepted, one of that release.
meets :: M.Map Text DatabaseConfig -> Text -> Requirement -> Bool
meets configs name req = case reqSubstitute req of
    Just substitute -> substitute == name
    Nothing -> any (sameRelease (reqRelease req)) (dcRelease =<< M.lookup name configs)

-- | Whether a database may link to the one named.
admits :: M.Map Text DatabaseConfig -> DatabaseConfig -> Text -> Bool
admits configs config name = null (dcRequires config) || any (meets configs name) (dcRequires config)

-- | The databases its requirements name, loaded before it so its links can reach them.
requiredDatabases :: M.Map Text DatabaseConfig -> DatabaseConfig -> [Text]
requiredDatabases configs config =
    [name | not (null (dcRequires config)), name <- M.keys configs, name /= dcName config, admits configs config name]

-- | How a required release stands in this engine.
data Satisfaction
    = -- | One or more databases of that release, every one holding the same data
      Satisfied
    | -- | The database its reader accepted in its place
      Substituted
    | -- | None; the databases named are of a release of the same name, another version or system model, which its reader may accept instead
      Missing
    deriving (Show, Eq, Enum, Bounded)

satisfactionCode :: Satisfaction -> Text
satisfactionCode Satisfied = "satisfied"
satisfactionCode Substituted = "substituted"
satisfactionCode Missing = "missing"

instance ToJSON Satisfaction where
    toJSON = toJSON . satisfactionCode

instance ToSchema Satisfaction where
    declareNamedSchema _ = pure (codeSchema "Satisfaction" satisfactionCode)

-- | A required release, how it stands, and the databases that state names.
data RequiredRelease = RequiredRelease
    { rrRelease :: !Release
    , rrState :: !Satisfaction
    , rrDatabases :: ![Text]
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, ToSchema) via (Stripped RequiredRelease)

-- | Each release a database requires, and how this engine stands with it.
requiredReleases :: M.Map Text DatabaseConfig -> DatabaseConfig -> [RequiredRelease]
requiredReleases configs config = [standing req | req <- dcRequires config]
  where
    standing :: Requirement -> RequiredRelease
    standing req = case (reqSubstitute req, filter (\n -> meets configs n req) others) of
        (Just substitute, _) -> RequiredRelease (reqRelease req) Substituted [substitute]
        (Nothing, []) -> RequiredRelease (reqRelease req) Missing (sameName req)
        (Nothing, held) -> RequiredRelease (reqRelease req) Satisfied held

    others :: [Text]
    others = filter (/= dcName config) (M.keys configs)

    sameName :: Requirement -> [Text]
    sameName req = [n | n <- others, Just r <- [dcRelease =<< M.lookup n configs], sameReleaseName r (reqRelease req)]

-- | A reader's choice of a database in place of a release a database requires.
data Substitution = Substitution
    { subRelease :: !Release
    , subDatabase :: !Text
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, ToSchema) via (Stripped Substitution)

-- Closed: a misspelt field would be read as absent, and the substitution refused for a reason that is not the real one.
instance FromJSON Substitution where
    parseJSON = parseClosed

{- | Record a reader's choice of a database in place of a required release.
A release the database does not require is refused, as is the database itself.
-}
acceptSubstitute :: Release -> Text -> DatabaseConfig -> Either Text [Requirement]
acceptSubstitute release substitute config
    | substitute == dcName config = Left (dcName config <> " cannot stand in for a release it requires itself.")
    | not (any (sameRelease release . reqRelease) (dcRequires config)) = Left (dcName config <> " does not require " <> releaseLabel release <> ".")
    | otherwise = Right [if sameRelease release (reqRelease r) then r{reqSubstitute = Just substitute} else r | r <- dcRequires config]

releaseLabel :: Release -> Text
releaseLabel r = T.unwords (releaseName r : releaseVersion r : maybe [] (\m -> ["(" <> m <> ")"]) (releaseSystemModel r))
