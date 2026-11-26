{-# LANGUAGE OverloadedStrings #-}

{- | A skill an assistant can load: a name, a description it reads before
choosing to load it, and the instructions it then follows.

The engine serves each one as a @SKILL.md@ file and announces its header
alongside. A client compares the two and drops the skill at the first
difference, so the engine writes the header itself rather than reading one:
what it announces and what it serves come from the same two fields.
-}
module Skill (Skill, SkillSource (..), skillFrom, skillName, skillDescription, skillUri, skillFile, skillEntry, skillResource, skillContents) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString as BS
import Data.Char (isAsciiLower, isDigit, isPrint, ord)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Numeric (showHex)

import Database.Digest (textDigest)

-- | A skill that 'skillFrom' accepted; nothing else builds one.
data Skill = Skill
    { skillName :: !Text
    , skillDescription :: !Text
    , skillBody :: !Text
    }
    deriving (Eq, Show)

-- | A skill as a configuration or the engine writes it, before 'skillFrom' checks it.
data SkillSource = SkillSource
    { sourceName :: !Text
    , sourceDescription :: !Text
    , sourceText :: !Text
    }

{- | A skill from its name, its description and its instructions, or the
reason it cannot be served.

The name follows the rules of the skills extension. The description is
written into the header as a quoted string, which a YAML reader takes back
as written only when every character is printable; and a client cuts the
header at the first @---@ it meets, so a description cannot hold one.
-}
skillFrom :: SkillSource -> Either Text Skill
skillFrom SkillSource{sourceName = name, sourceDescription = description, sourceText = body}
    | T.null name || T.length name > 64 = Left ("A skill's name has 1 to 64 characters; got \"" <> name <> "\".")
    | not (T.all nameChar name) || T.isPrefixOf "-" name || T.isSuffixOf "-" name || T.isInfixOf "--" name =
        Left ("A skill's name holds lowercase letters, digits and single hyphens inside, as \"carbon-footprint\"; got \"" <> name <> "\".")
    | T.null (T.strip description) = Left ("The skill " <> name <> " has no description: an assistant reads it to decide whether to load the skill.")
    | T.length description > 1024 = Left ("The description of the skill " <> name <> " is longer than 1024 characters.")
    | Just (position, c) <- find (not . isPrint . snd) (zip [1 :: Int ..] (T.unpack description)) =
        Left ("The description of the skill " <> name <> " holds " <> codePoint c <> " at character " <> T.pack (show position) <> ", which is not printable: a tab, a line break, or an invisible joiner or mark.")
    | T.isInfixOf "---" description = Left ("The description of the skill " <> name <> " holds \"---\", which ends the header of the file it is served in.")
    | T.null (T.strip body) = Left ("The skill " <> name <> " has no text.")
    | BS.length (TE.encodeUtf8 (skillFile skill)) > maxFileBytes = Left ("The skill " <> name <> " is longer than 100 kB, the most a client loads.")
    | otherwise = Right skill
  where
    skill :: Skill
    skill = Skill{skillName = name, skillDescription = description, skillBody = body}
    nameChar :: Char -> Bool
    nameChar c = isAsciiLower c || isDigit c || c == '-'
    codePoint :: Char -> Text
    codePoint c = "U+" <> T.justifyRight 4 '0' (T.toUpper (T.pack (showHex (ord c) "")))

-- | The largest file a client loads, in bytes.
maxFileBytes :: Int
maxFileBytes = 100000

-- | Where the skill is read: its last segment before the file is its name.
skillUri :: Skill -> Text
skillUri s = "skill://volca/" <> skillName s <> "/SKILL.md"

-- | The file served: the header, then the instructions.
skillFile :: Skill -> Text
skillFile s =
    T.unlines ["---", "name: " <> quoted (skillName s), "description: " <> quoted (skillDescription s), "---", ""]
        <> skillBody s
  where
    -- A JSON string is a double-quoted YAML string read the same way, once 'skillFrom' has kept out what is not printable.
    quoted :: Text -> Text
    quoted = TL.toStrict . encodeToLazyText

-- | The entry of @skills/list@ and @skills/get@: the header and every file, with its digest and size.
skillEntry :: Skill -> Value
skillEntry s =
    object
        [ "uri" .= skillUri s
        , "frontmatter" .= object ["name" .= skillName s, "description" .= skillDescription s]
        , "resources" .= [object ["uri" .= skillUri s, "digest" .= ("sha256:" <> textDigest file), "size" .= BS.length (TE.encodeUtf8 file)]]
        ]
  where
    file :: Text
    file = skillFile s

-- | The entry of @resources/list@.
skillResource :: Skill -> Value
skillResource s = object ["uri" .= skillUri s, "name" .= skillName s, "description" .= skillDescription s, "mimeType" .= markdown]

-- | The file as @resources/read@ gives it.
skillContents :: Skill -> Value
skillContents s = object ["uri" .= skillUri s, "mimeType" .= markdown, "text" .= skillFile s]

markdown :: Text
markdown = "text/markdown"
