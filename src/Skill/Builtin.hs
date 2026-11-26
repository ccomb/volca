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
                T.unlines
                    [ "# Carbon footprint of a product"
                    , ""
                    , "1. Choose the database with `list_databases`: the one whose description, region and sector fit the product."
                    , "2. Find the process with `search_activities`, in the language of the database's names. Read every candidate, and keep each detail the person gave: variety, production mode (conventional, organic), quality, location, stage (at farm gate, at plant, at consumer). When no candidate holds them, search again with those words."
                    , "   A generic process (\"Cow milk {FR}\") is not the specific one (\"Cow milk, organic, at farm gate {FR}\"), even when their scores are close. When two candidates fit, say which you took and why, or ask."
                    , "3. Choose the method collection with `list_methods`: the one the person names, otherwise the Environmental Footprint (EF) collection itself rather than a variant adapted for a particular tool. Score the process with `score_activity`: one call gives every impact category of the method collection. Take the total climate change impact category, and its parts when the method collection has them: fossil, biogenic, land use and land use change."
                    , "4. The score is for the functional unit it returns: convert it to what the person asked, and say so."
                    , "5. Answer with the total and its unit, then each part on its own line. Name the process and its location, the stage it stops at, the database, the method collection, and give the `web_url` link. When the method collection has no parts, give the total alone and say so."
                    , ""
                    , "If no process fits, say so, and name the closest one and how it differs. Never replace the database's answer with a web estimate."
                    ]
            }

pefHotspots :: Either Text Skill
pefHotspots =
    skillFrom
        SkillSource
            { sourceName = "pef-hotspots"
            , sourceDescription = "Find the most relevant impact categories, processes, and emissions and resources of a product with the PEF method (EF single score, 80 % rule of Recommendation (EU) 2021/2279). Use when asked what drives a product's environmental footprint, for a PEF study or a hotspot analysis."
            , sourceText =
                T.unlines
                    [ "# Hotspots of a product, PEF method"
                    , ""
                    , "1. Find the process as for any score (`list_databases`, `search_activities`), and say which one you took."
                    , "2. With `list_scoring_sets`, find the EF single score set. When none is loaded, say so and stop: never weight the impact categories yourself."
                    , "3. Score the process with `score_activity`, with `exchange_contributions` set to true: step 7 reads them."
                    , "4. Most relevant impact categories: rank the impact categories by their weighted contribution to the single score, in absolute value, and keep them, largest first, until they add up to at least 80 %. Keep at least three."
                    , "5. For each of them, `get_contributing_activities`: keep the processes, largest first in absolute value, until they add up to more than 80 % of that impact category. Compute the shares yourself from the absolute values: the shares the tool returns keep their sign. Raise `limit` until the rows cover the total."
                    , "6. For each of them, `get_contributing_flows`: the same rule on emissions and resources. Say that these are the emissions of the whole chain, not of each process."
                    , "7. Life cycle stages: group the `exchange_contributions` of `score_activity` (ingredients, transport, packaging...), and say how you grouped."
                    , "8. Answer with a table per step: name, share, cumulated share."
                    , ""
                    , "When a licence hides the detail of a database, say which lists are incomplete."
                    ]
            }
