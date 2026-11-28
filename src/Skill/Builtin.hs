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
builtinSkills = sequence [carbonFootprint, pefHotspots, processFromDocument]

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
                        <> [ "3. " <> chooseTheCollection <> " Score the process with `score_activity`, with `exchange_contributions` set to true and `top_flows` set to 0 (step 6 gives the flows): steps 4 and 7 read its answer. In its `scoringShares`, take the EF single score set: `PEF`, or a set whose name says EF normalisation and weighting; never a set made for a particular tool, nor a carbon footprint set. When there is none, find one with `list_scoring_sets`, score again with its method collection, and say which you took; when no method collection holds one, say so and stop: never weight the impact categories yourself."
                           , "4. Most relevant impact categories: read that set's single score in `scoringShares`. Its rows come largest first: keep them until `cumulativeSharePct` reaches at least 80, and at least three. Report each one's `sharePct`, which keeps its sign as the PEF method reads the impact categories, beside `cumulativeSharePct`, which adds absolute values; when one is negative, say so."
                           , "5. For each of them, `get_contributing_activities` with `cover_percent: 80`: the rows returned are the most relevant processes, the PEF rule on absolute values already applied. Report each one's `absolute_percent` and `cumulative_percent`, not the signed `contribution_percent`. When the last row's `cumulative_percent` is not above 80, the list stopped at its cap: say it is incomplete."
                           , "6. For each of them, `get_contributing_flows` with `cover_percent: 80` and `by_substance: true`: the same rule on emissions and resources, each substance gathered over its sub-compartments. Report each one's `absolute_percent` and `cumulative_percent`, not the signed `contribution_percent`. Say that these are the emissions of the whole chain, not of each process."
                           , "7. Life cycle stages: group the `exchangeContributions` of `score_activity` by the line names in `lineNames` (ingredients, transport, packaging...), add their signed shares, and say how you grouped."
                           , "8. Answer with a table per step: name, share, cumulated share, with the fields steps 4 to 7 name. Name the process and its location, the database, the method collection, and give the `web_url` link."
                           , ""
                           , "When a licence hides the detail of a database (`withheld_databases`), say which lists are incomplete."
                           ]
            }

processFromDocument :: Either Text Skill
processFromDocument =
    skillFrom
        SkillSource
            { sourceName = "process-from-document"
            , sourceDescription = "Turn a life cycle assessment report, a research paper or a picture of an inventory table into processes of a database: every level of the chain the document gives, each input linked to a supplier, the document named as the source. Use when given a PDF, a paper or an image holding an inventory (inputs and outputs with their amounts) to write into the database."
            , sourceText =
                T.unlines
                    [ "# Processes from a document"
                    , ""
                    , "Run every step before you answer. Every amount comes from the document, or from a computation or a general fact you name: never from memory of another study, nor from the web. Write a process for every inventory the document gives along the product's chain, not for the product alone."
                    , ""
                    , "1. Read the whole document. List each inventory it gives: the product, the amount the lines are stated for, the table and the page. An inventory lists inputs and outputs with their amounts. Impact results (kg CO2-eq, points, MJ eq) and a list of the datasets used are not an inventory."
                    , "   - No inventory with amounts: say so, say what the document holds instead, and stop. Never work a recipe back from impact results."
                    , "   - Inventories of several products that do not feed one another: take the one the person named; otherwise list them and ask which."
                    , "   - When the document also gives the inventory of an input of the product (the pulp of a fibre, the wood chips of that pulp), that input is a process of its own, at every level the document describes. Write them from upstream to downstream, each with steps 2 and 4 to 7, into the one database step 3 gives: each process takes the one before it by the process_id it got. Never put a proxy where the document gives the inventory, even when linking the levels needs a conversion: take a density or a yield from general knowledge, and name it."
                    , "2. Write each recipe as a table: one row per input and per output, with the name as the document writes it, the amount, the unit, and where the number comes from: \"document, table 2, page 9\", \"computed: <formula>\" or \"general knowledge: <fact>\" (a density, a heating value). Add up the rows the document gives twice for one input (the same input at two stages), and keep each stage in the source. Keep the document's reference amount and unit for the product (1 t, 1 kg, 1000 m2)."
                    , "3. The database to write into is the one the person names, when it takes writing. When they name none, or the engine refuses to write it because it is shared background data, create one with `create_database`: `new_name` after the study (\"lyocell-smith-2024\"), `depends` the database that holds the suppliers, the one the person names or the one `list_databases` shows whose region and sector fit. When the engine answers that the name you gave `create_database` is taken, that database holds work you did not do here: never write into it, and create one under another name. When the engine refuses for a quota, or offers no tool to write because the instance is read-only, give the recipes, say why nothing was written, and stop. When `create_activities` answers that the database the person named is not loaded, `load_database` it, then write."
                    , "4. Inputs: find each supplier with `search_activities` in the database of the suppliers (a database you created holds none, except the processes you wrote from the document), in the language of the database's names. Read the candidates: prefer the document's location, then a regional or global one. When none is the input, search again with a shorter word, its chemical family or a near substance (a solvent's base molecule), then with a generic product of its kind (\"chemicals organic\", \"chemicals inorganic\"); take the closest one and write \"proxy: <why>\" in the line's comment. Every input the document gives an amount for gets a supplier, a proxy when nothing closer exists: a line left out weighs zero, which is further from the truth than a proxy."
                    , "   Give each amount in the unit of its supplier's product. Prefer a supplier stated in the document's unit (energy in MJ: a heat process; in kWh: electricity). Otherwise convert, and write the conversion in the line's comment: \"document: 7.2 MJ; 1 kWh = 3.6 MJ\"."
                    , "5. Outputs: the product is the reference product. Wastewater and solid waste go in `waste_outputs`, to the treatment process `search_activities` finds. Emissions to air, water or soil the document gives go in `biosphere`, with `direction` \"emission\" (\"resource\" for what is taken from nature), by name, compartment and unit; find the database's name for each with `search_flows` in the database of the suppliers."
                    , "6. Write the process with `create_activities`:"
                    , "   - `name`: the product and its route, with the document's author and year, e.g. \"Glass bottle, production (Smith 2024)\";"
                    , "   - `location`: the one the document gives; GLO when it gives none, and say so;"
                    , "   - `product_name`, `product_amount`, `product_unit`: the document's reference amount, e.g. 1 and \"t\";"
                    , "   - `description`, one string per paragraph. The first one names the document: \"Source: <title>, <authors>, <year>, file <file name>\". Then its reference amount, and what it leaves out;"
                    , "   - `inputs` and `waste_outputs`: {provider, amount, unit, comment}, provider being the supplier's process_id;"
                    , "   - each line's `comment`: its source from step 2, the conversion from step 4, and \"proxy: ...\" when it is one;"
                    , "   - a line the document names without an amount: leave `amount` out, and the person types it."
                    , "   - `source` beside `activities`: {document: <file name>, page}, and a `note` saying the process is read from that document;"
                    , "   When the call refuses a unit, write the amount in the supplier's unit, convert it, and put the conversion in the comment."
                    , "7. Read the new process with `get_activity`, and compare its lines with the recipe table: each row has its line, with its amount and unit. Fix a difference with `edit_exchanges`."
                    , "8. " <> chooseTheCollection <> " Score the last process of the chain, the document's product, with `score_activity`, with `top_flows: 0`. Take the total climate change impact category. The score is for one unit of the product, the `functional_unit` it returns: multiply it by the document's reference amount when that is not 1, and say so."
                    , "9. Answer with:"
                    , "   - the database written into, and whether you created it;"
                    , "   - each process's name and process_id, and the `web_url` link `score_activity` gave;"
                    , "   - each recipe table, with the supplier taken for each line;"
                    , "   - each proxy, each line left out, and why;"
                    , "   - what the document does not say: location, year, allocation, transport;"
                    , "   - the climate change total, for the document's reference amount."
                    ]
            }

{- | The first two steps of every skill that finds a product to score, written once:
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

{- | Which method collection scores a product. The engine takes the instance's
default when the call names none, so a small model has no choice to get wrong.
-}
chooseTheCollection :: Text
chooseTheCollection = "Leave `collection` out unless the person names one: the engine then uses the method collection this instance offers first, and `basis` names it. If the engine refuses because several method collections are loaded, take the Environmental Footprint (EF) one it lists (often EF-3.1), not a variant adapted for a particular tool nor `plain-indicators`. Once you use a method collection other than the one the engine took (named by the person, taken after a refusal, or holding the set you looked for), pass it in every later call."
