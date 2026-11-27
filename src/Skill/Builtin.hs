{-# LANGUAGE OverloadedStrings #-}

{- | The skills every engine serves, and how a configuration's own skills
join them.
-}
module Skill.Builtin (builtinSkills, servedSkills) where

import Data.Text (Text)
import qualified Data.Text as T

import Skill (Skill, SkillSource (..), skillFrom, skillName)

{- | The skills built into the engine. They go through 'skillFrom' like a
configuration's own, so a text written here that breaks a rule stops the
engine at start, with the reason, rather than being served and dropped by
the client.
-}
builtinSkills :: Either Text [Skill]
builtinSkills = sequence [carbonFootprint, pefHotspots]

{- | The skills a configuration declares, then each built-in one none of them
replaces by taking its name.
-}
servedSkills :: [Skill] -> Either Text [Skill]
servedSkills own = (own <>) . filter ((`notElem` map skillName own) . skillName) <$> builtinSkills

carbonFootprint :: Either Text Skill
carbonFootprint =
    skillFrom
        SkillSource
            { sourceName = "carbon-footprint"
            , sourceDescription = "Carbon footprint of a product (empreinte carbone, CO2-eq, GHG) from a life-cycle database, with its fossil, biogenic and land use parts and its source. Use when asked for the climate impact of a food, material, packaging, energy or transport."
            , sourceText =
                T.unlines $
                    [ "# Carbon footprint of a product"
                    , ""
                    ]
                        <> findTheProcess
                        <> [ "3. " <> chooseTheCollection <> " Score the process with `score_activity`: one call gives every impact category of the method collection. Take the total climate change impact category, and its parts when the method collection has them: fossil, biogenic, land use and land use change."
                           , "4. The score is for the functional unit it returns: convert it to what the person asked, and say so."
                           , "5. Answer with the total and its unit, then each part on its own line. Name the process and its location, the stage it stops at, the database, the method collection, and give the `web_url` link. When the method collection has no parts, give the total alone and say so."
                           , ""
                           , "Never replace the database's answer with a web estimate."
                           ]
            }

pefHotspots :: Either Text Skill
pefHotspots =
    skillFrom
        SkillSource
            { sourceName = "pef-hotspots"
            , sourceDescription = "Find the most relevant impact categories, processes, and emissions and resources of a product with the PEF method (EF single score, 80 % rule of Recommendation (EU) 2021/2279). Use when asked what drives a product's environmental footprint, for a PEF study or a hotspot analysis."
            , sourceText =
                T.unlines $
                    [ "# Hotspots of a product, PEF method"
                    , ""
                    , "Run every step before you answer."
                    , ""
                    ]
                        <> findTheProcess
                        <> [ "3. " <> chooseTheCollection <> " With `list_scoring_sets` and that `collection`, find the EF single score set of that collection, otherwise of another loaded one, and say which collection holds it. When none is loaded, say so and stop: never weight the impact categories yourself."
                           , "4. Score the process with `score_activity`, with `scoring_sets` set to that set alone, `exchange_contributions` set to true and `top_flows` set to 0 (step 7 gives the flows): steps 5 and 8 read its answer."
                           , "5. Most relevant impact categories: read that set's single score in `scoringShares`. Its rows come largest first: keep them until `cumulativeSharePct` reaches at least 80, and at least three. Report each one's `sharePct`, which keeps its sign as the PEF method reads the categories; when one is negative, say so."
                           , "6. For each of them, `get_contributing_activities` with `cover_percent: 80`: the rows returned are the most relevant processes, the PEF rule on absolute values already applied. When the last row's `cumulative_percent` is not above 80, the list stopped at its cap: say it is incomplete."
                           , "7. For each of them, `get_contributing_flows` with `cover_percent: 80` and `by_substance: true`: the same rule on emissions and resources, each substance gathered over its sub-compartments. Say that these are the emissions of the whole chain, not of each process."
                           , "8. Life cycle stages: group the `exchangeContributions` of `score_activity` by the line names in `lineNames` (ingredients, transport, packaging...), add their signed shares, and say how you grouped."
                           , "9. Answer with a table per step: name, share, cumulated share, as the tools return them. Name the process and its location, the database, the method collection, and give the `web_url` link."
                           , ""
                           , "When a licence hides the detail of a database (`withheld_databases`), say which lists are incomplete."
                           ]
            }

{- | The first two steps of every skill that scores a product, written once:
a client may load one skill alone, so each carries them whole.
-}
findTheProcess :: [Text]
findTheProcess =
    [ "1. Choose the database with `list_databases`: the one whose description, region and sector fit the product."
    , "2. Find the process with `search_activities`, in the language of the database's names. Read every candidate, and keep each detail the person gave: variety, production mode (conventional, organic), quality, location, stage (at farm gate, at plant, at consumer). When no candidate holds them, search again with those words."
    , "   A generic process (\"Cow milk {FR}\") is not the specific one (\"Cow milk, organic, at farm gate {FR}\"), even when their scores are close. When two candidates fit, say which you took and why, or ask."
    , "   A packaging, an ingredient or a by-product of the product is not the product: when every candidate is one of those, search again with another spelling or another word for the product."
    , "   If no process fits, say so, name the closest one and how it differs, and stop there."
    ]

-- | Which method collection scores a product, when the person names none.
chooseTheCollection :: Text
chooseTheCollection = "Choose the method collection with `list_methods`: the one the person names, otherwise the plain Environmental Footprint (EF) collection, often named by its acronym and version (EF-3.1), rather than a variant whose name says it is adapted for a particular tool."
