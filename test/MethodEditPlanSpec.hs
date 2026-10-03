{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MethodEditPlanSpec (spec) where

import Config (MethodPatch (..), MethodPatchMatch (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Method.EditPlan
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

ammoniaPatch :: MethodPatch
ammoniaPatch = MethodPatch Nothing (MethodPatchMatch Nothing (Just "Ammonia") Nothing Nothing Nothing) (ScaleBy 2)

target :: Int -> Maybe Double -> FactorTarget
target flow = FactorTarget (uuid 100) (uuid flow) Nothing

refusal :: Either T.Text a -> String
refusal = either T.unpack (const "accepted")

-- | Plan a change, apply it, then apply its inverse.
roundTrip :: MethodCollection -> FactorEdit -> Either T.Text MethodCollection
roundTrip start edit = do
    (op, _) <- planEdit start edit
    changed <- applyMethodOp start op
    inverseOf changed op >>= \case
        UndoWith inverse -> applyMethodOp changed inverse
        UndoSelector patch -> applyMethodOp changed (RestoreFactors (restoreOf patch start))

spec :: Spec
spec = describe "planning a change to a method collection" $ do
    it "takes the one factor at an address, without being told its value" $
        fmap snd (planEdit collection (SetValue (target 2 Nothing) 4))
            `shouldBe` Right (EditEffect 1 (Just 3) (Just 4))

    it "lets the value choose between two factors at one address" $
        fmap fst (planEdit collection (SetValue (target 1 (Just 1.1619)) 2))
            `shouldBe` Right (SetFactor (uuid 100) turpentineB 2)

    it "asks for the value when two factors differ only by it, naming both" $ do
        refusal (planEdit collection (SetValue (target 1 Nothing) 2)) `shouldContain` "8.399"
        refusal (planEdit collection (SetValue (target 1 Nothing) 2)) `shouldContain` "1.1619"

    it "refuses two identical factors one by one, and says a selector reaches them" $
        let twins = collection{mcMethods = [ecotox{methodFactors = [turpentineA, turpentineA]}]}
         in refusal (planEdit twins (Remove (target 1 (Just 8.399)))) `shouldContain` "selector"

    it "says a selector reaches factors that share the value named, rather than asking for it again" $
        let industrial = turpentineA{mcfCompartment = Just (Compartment "soil" "industrial" "")}
            shared = collection{mcMethods = [ecotox{methodFactors = [turpentineA, industrial]}]}
            refused = refusal (planEdit shared (SetValue (target 1 (Just 8.399)) 2))
         in do
                refused `shouldContain` "selector"
                refused `shouldNotContain` "Name the value"

    it "refuses a value that would make two factors at one place identical" $
        refusal (planEdit collection (SetValue (target 1 (Just 1.1619)) 8.399)) `shouldContain` "identical"

    it "names the later line that changed the factor an undo would give back" $ do
        let lines' = [MethodLine (SetFactor (uuid 100) ammonia 4) Change, MethodLine (SetFactor (uuid 100) ammonia{mcfValue = 4} 5) Change]
            now = collection{mcMethods = [ecotox{methodFactors = [turpentineA, ammonia{mcfValue = 5}, turpentineB]}]}
        T.unpack (blockedUndo now lines' 1 "replay failed") `shouldContain` "undo line 2 first"
        blockedUndo now (take 1 lines') 1 "replay failed" `shouldBe` "replay failed"

    it "names a later selector that changed the factor an undo would give back" $ do
        let lines' = [MethodLine (SetFactor (uuid 100) ammonia 4) Change, MethodLine (PatchFactors ammoniaPatch 1) Change]
        T.unpack (blockedUndo collection lines' 1 "replay failed") `shouldContain` "line 2"

    it "names the values an address holds when the stated one is not there" $
        refusal (planEdit collection (SetValue (target 2 (Just 5)) 1)) `shouldContain` "3.0"

    it "says when an address holds no factor at all" $
        refusal (planEdit collection (Remove (target 9 Nothing))) `shouldContain` "no factor"

    it "refuses to add at an address a factor already holds" $
        refusal (planEdit collection (Add (uuid 100) ammonia{mcfValue = 7})) `shouldContain` "change its value"

    it "adds a factor at a new address, at the end" $
        fmap fst (planEdit collection (Add (uuid 100) ammonia{mcfConsumerLocation = Just "FR"}))
            `shouldBe` Right (AddFactor (uuid 100) Nothing ammonia{mcfConsumerLocation = Just "FR"})

    it "refuses a selector that touches nothing" $
        let nothing = ammoniaPatch{mpMatch = (mpMatch ammoniaPatch){mpmFlowName = Just "Neon"}}
         in refusal (planEdit collection (Patch nothing)) `shouldContain` "touches no factor"

    it "undoes the latest change in effect, then the one before" $ do
        let edit :: Double -> MethodLine
            edit v = MethodLine (SetFactor (uuid 100) ammonia v) Change
            undo k = MethodLine (SetFactor (uuid 100) ammonia 0) (Undoing k)
        undoTarget [edit 1, edit 2] Nothing `shouldBe` Right 2
        undoTarget [edit 1, edit 2, undo 2] Nothing `shouldBe` Right 1
        refusal (undoTarget [edit 1, edit 2, undo 2, undo 1] Nothing) `shouldContain` "no change left"
        inEffect [edit 1, edit 2, undo 2, undo 3] `shouldBe` [True, True, False, True]

    it "never reaches what a copy took from the configuration, unless named" $ do
        let seed = MethodLine (SetGlobalMethods [] ["Land use"]) TakenFromConfiguration
            edit = MethodLine (SetFactor (uuid 100) ammonia 1) Change
        undoTarget [seed] Nothing `shouldBe` Left "there is no change left to undo"
        undoTarget [seed, edit] Nothing `shouldBe` Right 2
        undoTarget [seed, edit] (Just 1) `shouldBe` Right 1

    it "undoes an undo when asked for it by its line" $ do
        let lines' = [MethodLine (SetFactor (uuid 100) ammonia 1) Change, MethodLine (SetFactor (uuid 100) ammonia{mcfValue = 1} 3) (Undoing 1)]
        undoTarget lines' (Just 2) `shouldBe` Right 2
        refusal (undoTarget lines' (Just 1)) `shouldContain` "already undone"
        refusal (undoTarget lines' (Just 3)) `shouldContain` "no line 3"

    it "inverts a change of value, an addition and a removal" $ do
        inverseOf collection (SetFactor (uuid 100) ammonia 4) `shouldBe` Right (UndoWith (SetFactor (uuid 100) ammonia{mcfValue = 4} 3))
        inverseOf collection (RemoveFactor (uuid 100) 1 ammonia) `shouldBe` Right (UndoWith (AddFactor (uuid 100) (Just 1) ammonia))
        inverseOf collection (AddFactor (uuid 100) Nothing ammonia) `shouldBe` Right (UndoWith (RemoveFactor (uuid 100) 1 ammonia))

    it "undoes the removal of one of two factors at one address" $
        roundTrip collection (Remove (target 1 (Just 8.399))) `shouldBe` Right collection

    it "undoes every kind of change it plans back to where it started" $
        mapM_
            (\edit -> roundTrip collection edit `shouldBe` Right collection)
            [ SetValue (target 2 Nothing) 0.30000000000000004
            , Remove (target 2 Nothing)
            , Add (uuid 100) ammonia{mcfConsumerLocation = Just "FR"}
            , Patch ammoniaPatch
            , Patch ammoniaPatch{mpOp = SetValueTo 0}
            ]

    it "restores exactly the values a selector replaced" $ do
        let setAll = MethodPatch Nothing (MethodPatchMatch Nothing (Just "turpentine") Nothing Nothing Nothing) (SetValueTo 0)
        roundTrip collection (Patch setAll) `shouldBe` Right collection

    it "seeds a copy's journal with what the configuration adds, in the order it applies" $
        seedLines [] [ammoniaPatch] ["Ecotoxicity, freshwater"] collection
            `shouldBe` [PatchFactors ammoniaPatch 1, SetGlobalMethods [] ["Ecotoxicity, freshwater"]]

    it "plans a new category, with no factor, named by the identifier it is given" $
        fmap fst (planCategoryEdit (uuid 200) collection (NewCategory (CategoryDraft " Land use " "Pt" Nothing Nothing)))
            `shouldBe` Right (AddCategory Nothing ecotox{methodId = uuid 200, methodName = "Land use", methodUnit = "Pt", methodCategory = "Land use", methodFactors = []} Regionalized)

    it "refuses a new category with no name, no unit, or a name already there once its edges are trimmed" $ do
        let draft name unit = NewCategory (CategoryDraft name unit Nothing Nothing)
        refusal (planCategoryEdit (uuid 200) collection (draft "  " "Pt")) `shouldContain` "name"
        refusal (planCategoryEdit (uuid 200) collection (draft "Land use" "")) `shouldContain` "unit"
        refusal (planCategoryEdit (uuid 200) collection (draft "Ecotoxicity, freshwater " "Pt")) `shouldContain` "already named"

    it "plans a rename with the name it replaces, and refuses one that changes nothing, a name in use, or one a second category shares" $ do
        fmap fst (planCategoryEdit (uuid 1) collection (Rename (uuid 100) "Freshwater ecotoxicity"))
            `shouldBe` Right (RenameCategory (uuid 100) "Ecotoxicity, freshwater" "Freshwater ecotoxicity")
        refusal (planCategoryEdit (uuid 1) collection (Rename (uuid 100) "Ecotoxicity, freshwater")) `shouldContain` "already named"
        let dangling = collection{mcUnregionalized = ["Land use"]}
        refusal (planCategoryEdit (uuid 1) dangling (Rename (uuid 100) "Land use")) `shouldContain` "no impact category has"
        let twins = collection{mcMethods = [ecotox, landUse{methodName = "Ecotoxicity, freshwater"}]}
        refusal (planCategoryEdit (uuid 1) twins (Rename (uuid 100) "Other") >>= applyMethodOp twins . fst) `shouldContain` "several impact categories"

    it "plans a change of unit with the unit it replaces" $
        fmap fst (planCategoryEdit (uuid 1) collection (ChangeUnit (uuid 100) "PAF.m3.day"))
            `shouldBe` Right (SetCategoryUnit (uuid 100) "CTUe" "PAF.m3.day")

    it "plans a removal with the category's position, its factors and its rank among the unregionalized ones" $
        let two = MethodCollection [landUse, ecotox] [] ["Ecotoxicity, freshwater"]
         in fmap fst (planCategoryEdit (uuid 1) two (Delete (uuid 100))) `shouldBe` Right (RemoveCategory 1 ecotox (UnregionalizedAt 0))

    it "undoes every change it plans to a category back to where it started" $
        let two = MethodCollection [landUse, ecotox] [weighing "Land use"] ["Ecotoxicity, freshwater"]
            back edit = do
                (op, _) <- planCategoryEdit (uuid 300) two edit
                changed <- applyMethodOp two op
                inverseOf changed op >>= \case
                    UndoWith inverse -> applyMethodOp changed inverse
                    UndoSelector _ -> Left "a category's change is never undone by a selector"
         in mapM_
                (\edit -> back edit `shouldBe` Right two)
                [ NewCategory (CategoryDraft "Water use" "m3" (Just "Water") (Just "AWARE"))
                , Rename (uuid 200) "Land occupation"
                , ChangeUnit (uuid 100) "PAF.m3.day"
                , Delete (uuid 100)
                ]

    it "names the later line that added a factor to a category whose addition is undone" $ do
        let added = landUse{methodFactors = []}
            lines' = [MethodLine (AddCategory Nothing added Regionalized) Change, MethodLine (AddFactor (uuid 200) Nothing ammonia) Change]
            now = collection{mcMethods = [ecotox, added{methodFactors = [ammonia]}]}
        T.unpack (blockedUndo now lines' 1 "replay failed") `shouldContain` "undo line 2 first"

    it "names the later line that removed the category of a factor change being undone" $ do
        let lines' = [MethodLine (SetFactor (uuid 100) ammonia 4) Change, MethodLine (RemoveCategory 0 ecotox Regionalized) Change]
        T.unpack (blockedUndo (MethodCollection [] [] []) lines' 1 "replay failed") `shouldContain` "undo line 2 first"

    it "does not blame a later rename for a factor change it did not block" $ do
        let lines' = [MethodLine (SetFactor (uuid 100) ammonia 4) Change, MethodLine (RenameCategory (uuid 100) "Ecotoxicity, freshwater" "Other") Change]
        blockedUndo collection lines' 1 "replay failed" `shouldBe` "replay failed"
