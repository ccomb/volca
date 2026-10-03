{-# LANGUAGE OverloadedStrings #-}

module MethodJournalSpec (spec) where

import Config (MethodPatch (..), MethodPatchMatch (..))
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString.Lazy as BL
import Data.JournalFile (Entry (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Method.Journal
import Method.Types
import Test.Hspec
import Types (PatchOp (..))

uuid :: Int -> UUID.UUID
uuid n = fromMaybe UUID.nil (UUID.fromText ("00000000-0000-0000-0000-" <> T.justifyRight 12 '0' (T.pack (show n))))

cf :: Int -> T.Text -> Double -> MethodCF
cf flow name value =
    MethodCF
        { mcfFlowRef = uuid flow
        , mcfFlowName = name
        , mcfDirection = Output
        , mcfValue = value
        , mcfCompartment = Just (Compartment "soil" "agricultural" "")
        , mcfCAS = Nothing
        , mcfUnit = "kg"
        , mcfConsumerLocation = Nothing
        }

-- Two factors one flow writes twice at one place, with two values (the
-- shape a SimaPro file leaves when two source flows fold into one name).
turpentineA, turpentineB, ammonia :: MethodCF
turpentineA = cf 1 "turpentine" 8.399
turpentineB = cf 1 "turpentine" 1.1619
ammonia = cf 2 "Ammonia" 3.0

ecotox :: Method
ecotox =
    Method
        { methodId = uuid 100
        , methodName = "Ecotoxicity, freshwater"
        , methodDescription = Nothing
        , methodUnit = "CTUe"
        , methodCategory = "Ecotoxicity, freshwater"
        , methodMethodology = Nothing
        , methodFactors = [turpentineA, ammonia, turpentineB]
        }

collection :: MethodCollection
collection = MethodCollection [ecotox] [] []

-- | A set weighing one category under the variable « eco ».
weighing :: T.Text -> ScoringSet
weighing category = ScoringSet "Single" "Pt" (M.fromList [("eco", category)]) M.empty M.empty M.empty (M.fromList [("eco", 1)]) (M.fromList [("Single score", "eco")]) Nothing M.empty CreatedInJournal

landUse :: Method
landUse = ecotox{methodId = uuid 200, methodName = "Land use", methodUnit = "Pt", methodCategory = "Land use", methodFactors = [ammonia]}

factorsOf :: MethodCollection -> [MethodCF]
factorsOf = concatMap methodFactors . mcMethods

line :: MethodOp -> Entry MethodLine
line op = Entry "2026-10-02T09:00:00Z" (MethodLine op Change)

ammoniaPatch :: MethodPatch
ammoniaPatch = MethodPatch Nothing (MethodPatchMatch Nothing (Just "Ammonia") Nothing Nothing Nothing) (ScaleBy 2)

refusal :: Either T.Text a -> String
refusal = either T.unpack (const "applied")

spec :: Spec
spec = describe "a method collection's journal" $ do
    it "sets the one factor whose value the line names, among two at one place" $
        fmap factorsOf (applyMethodOp collection (SetFactor (uuid 100) turpentineB 2.0))
            `shouldBe` Right [turpentineA, ammonia, turpentineB{mcfValue = 2.0}]

    it "stops on a factor the source no longer has, naming it" $
        refusal (applyMethodOp collection (SetFactor (uuid 100) (cf 1 "turpentine" 5) 2)) `shouldContain` "turpentine"

    it "refuses two identical factors rather than pick one" $
        let twins = collection{mcMethods = [ecotox{methodFactors = [turpentineA, turpentineA]}]}
         in refusal (applyMethodOp twins (SetFactor (uuid 100) turpentineA 1)) `shouldContain` "twice"

    it "replays a selector only when it touches the number it recorded" $ do
        fmap factorsOf (applyMethodOp collection (PatchFactors ammoniaPatch 1))
            `shouldBe` Right [turpentineA, ammonia{mcfValue = 6.0}, turpentineB]
        refusal (applyMethodOp collection (PatchFactors ammoniaPatch 2)) `shouldContain` "recorded as touching 2"

    it "replays a selector that recorded zero, and touches nothing" $
        let nothing = ammoniaPatch{mpMatch = (mpMatch ammoniaPatch){mpmFlowName = Just "Neon"}}
         in applyMethodOp collection (PatchFactors nothing 0) `shouldBe` Right collection

    it "puts a removed factor back where it was" $
        case applyMethodOp collection (RemoveFactor (uuid 100) 1 ammonia) of
            Left err -> expectationFailure (T.unpack err)
            Right removed -> do
                factorsOf removed `shouldBe` [turpentineA, turpentineB]
                applyMethodOp removed (AddFactor (uuid 100) (Just 1) ammonia) `shouldBe` Right collection

    -- The undo of removing one of two factors at one address adds it back
    -- beside the other: a replay judges the position, never the address.
    it "puts back a removed factor beside another at the same address" $
        case applyMethodOp collection (RemoveFactor (uuid 100) 0 turpentineA) of
            Left err -> expectationFailure (T.unpack err)
            Right removed -> applyMethodOp removed (AddFactor (uuid 100) (Just 0) turpentineA) `shouldBe` Right collection

    it "refuses a removal at a position that holds another factor" $
        refusal (applyMethodOp collection (RemoveFactor (uuid 100) 0 ammonia)) `shouldContain` "position 0"

    it "refuses a position past the end" $
        refusal (applyMethodOp collection (AddFactor (uuid 100) (Just 9) ammonia)) `shouldContain` "past the end"

    it "restores values by position, and stops when the factor there is not the one recorded" $ do
        fmap factorsOf (applyMethodOp collection (RestoreFactors [Restore (uuid 100) 1 ammonia 1.5]))
            `shouldBe` Right [turpentineA, ammonia{mcfValue = 1.5}, turpentineB]
        refusal (applyMethodOp collection (RestoreFactors [Restore (uuid 100) 0 ammonia 1.5])) `shouldContain` "position"

    it "sets the unregionalized categories only from what the line found" $ do
        fmap mcUnregionalized (applyMethodOp collection (SetGlobalMethods [] ["Land use"])) `shouldBe` Right ["Land use"]
        refusal (applyMethodOp collection (SetGlobalMethods ["Water use"] [])) `shouldContain` "Water use"

    it "names the category a line addresses when there is none" $
        refusal (applyMethodOp collection (SetFactor (uuid 7) ammonia 1)) `shouldContain` UUID.toString (uuid 7)

    it "names the line a replay stops at" $
        refusal (replayMethodJournal collection [line (PatchFactors ammoniaPatch 1), line (SetFactor (uuid 100) ammonia 1)])
            `shouldContain` "journal line 2 (set-factor)"

    it "adds a category at the end, or at a position, and refuses an identifier or a name already there" $ do
        fmap (map methodName . mcMethods) (applyMethodOp collection (AddCategory Nothing landUse Regionalized))
            `shouldBe` Right ["Ecotoxicity, freshwater", "Land use"]
        fmap (map methodName . mcMethods) (applyMethodOp collection (AddCategory (Just 0) landUse Regionalized))
            `shouldBe` Right ["Land use", "Ecotoxicity, freshwater"]
        refusal (applyMethodOp collection (AddCategory Nothing landUse{methodId = uuid 100} Regionalized)) `shouldContain` "identifier"
        refusal (applyMethodOp collection (AddCategory Nothing landUse{methodName = "Ecotoxicity, freshwater"} Regionalized)) `shouldContain` "already named"

    it "refuses a new category whose name a scoring set already holds for no category" $
        refusal (applyMethodOp collection{mcScoringSets = [weighing "Land use"]} (AddCategory Nothing landUse Regionalized))
            `shouldContain` "the scoring set Single already names Land use"

    it "renames a category, and the scoring sets and unregionalized categories naming it follow" $
        let named = collection{mcScoringSets = [weighing "Ecotoxicity, freshwater"], mcUnregionalized = ["Ecotoxicity, freshwater"]}
            renamed = applyMethodOp named (RenameCategory (uuid 100) "Ecotoxicity, freshwater" "Freshwater ecotoxicity")
         in do
                fmap (map methodName . mcMethods) renamed `shouldBe` Right ["Freshwater ecotoxicity"]
                fmap (map ssVariables . mcScoringSets) renamed `shouldBe` Right [M.fromList [("eco", "Freshwater ecotoxicity")]]
                fmap mcUnregionalized renamed `shouldBe` Right ["Freshwater ecotoxicity"]
                (renamed >>= (`applyMethodOp` RenameCategory (uuid 100) "Freshwater ecotoxicity" "Ecotoxicity, freshwater")) `shouldBe` Right named

    it "stops a rename whose category no longer has the recorded name" $
        refusal (applyMethodOp collection (RenameCategory (uuid 100) "Ecotoxicity" "Other")) `shouldContain` "as recorded"

    it "refuses to rename a category whose name another one shares" $
        let twins = collection{mcMethods = [ecotox, landUse{methodName = "Ecotoxicity, freshwater"}]}
         in refusal (applyMethodOp twins (RenameCategory (uuid 100) "Ecotoxicity, freshwater" "Other")) `shouldContain` "several impact categories"

    it "changes a category's unit only from the one recorded" $ do
        fmap (map methodUnit . mcMethods) (applyMethodOp collection (SetCategoryUnit (uuid 100) "CTUe" "PAF.m3.day"))
            `shouldBe` Right ["PAF.m3.day"]
        refusal (applyMethodOp collection (SetCategoryUnit (uuid 100) "kg" "PAF.m3.day")) `shouldContain` "as recorded"

    it "removes a category with its place among the unregionalized ones, and puts it back there" $
        let two = MethodCollection [ecotox, landUse] [] ["Land use", "Ecotoxicity, freshwater"]
            removed = applyMethodOp two (RemoveCategory 0 ecotox (UnregionalizedAt 1))
         in do
                fmap (\c -> (map methodName (mcMethods c), mcUnregionalized c)) removed `shouldBe` Right (["Land use"], ["Land use"])
                (removed >>= \c -> applyMethodOp c (AddCategory (Just 0) ecotox (UnregionalizedAt 1))) `shouldBe` Right two

    it "refuses to remove a category a scoring set weighs, naming the set" $
        refusal (applyMethodOp collection{mcScoringSets = [weighing "Ecotoxicity, freshwater"]} (RemoveCategory 0 ecotox Regionalized))
            `shouldContain` "Single"

    it "stops a removal whose category is not the one recorded at its position" $ do
        refusal (applyMethodOp collection (RemoveCategory 0 ecotox{methodUnit = "kg"} Regionalized)) `shouldContain` "as recorded"
        refusal (applyMethodOp collection{mcUnregionalized = ["Ecotoxicity, freshwater"]} (RemoveCategory 0 ecotox Regionalized))
            `shouldContain` "did not record"

    it "writes every verb and every kind of line in words it reads back" $
        let set = ScoringSet "Single" "Pt" (M.fromList [("a", "Ecotoxicity, freshwater")]) M.empty M.empty M.empty (M.fromList [("a", 1)]) (M.fromList [("Single score", "a")]) Nothing M.empty CreatedInJournal
            ops =
                [ SetFactor (uuid 100) turpentineB 2
                , PatchFactors ammoniaPatch{mpDescription = Just "why"} 1
                , PatchFactors ammoniaPatch{mpOp = SetValueTo 0} 0
                , RestoreFactors [Restore (uuid 100) 1 ammonia 1.5]
                , SetGlobalMethods [] ["Land use"]
                , RemoveFactor (uuid 100) 1 ammonia{mcfConsumerLocation = Just "FR", mcfCAS = Just "7664-41-7"}
                , AddFactor (uuid 100) Nothing ammonia{mcfCompartment = Nothing, mcfDirection = Input}
                , AddFactor (uuid 100) (Just 0) ammonia
                , CreateScoringSet set
                , RemoveScoringSet set{ssDisplayMultiplier = Just 1000}
                , AddCategory Nothing landUse{methodFactors = []} Regionalized
                , AddCategory (Just 1) ecotox{methodDescription = Just "why", methodMethodology = Just "EF"} (UnregionalizedAt 0)
                , RenameCategory (uuid 100) "Ecotoxicity, freshwater" "Freshwater ecotoxicity"
                , SetCategoryUnit (uuid 100) "CTUe" "PAF.m3.day"
                , RemoveCategory 0 ecotox (UnregionalizedAt 2)
                ]
            kinds = cycle [Change, Undoing 1, TakenFromConfiguration]
            entries = zipWith (\op kind -> Entry "t" (MethodLine op kind)) ops kinds
         in mapM_ (\e -> decodeStrict (BL.toStrict (encode e)) `shouldBe` Just e) entries

    it "refuses a line that both undoes another and comes from the configuration" $
        (decodeStrict "{\"v\":1,\"at\":\"t\",\"op\":\"set-global-methods\",\"before\":[],\"after\":[],\"undoes\":1,\"seed\":true}" :: Maybe (Entry MethodLine))
            `shouldBe` Nothing
