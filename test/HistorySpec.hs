{-# LANGUAGE OverloadedStrings #-}

module HistorySpec (spec) where

import qualified Data.Text as T
import qualified Data.UUID as UUID
import Test.Hspec

import Data.JournalFile (Entry (..))
import Database.Author (AuthoredActivity (..), ExchangeEdit (..), ExchangeSelector (..))
import Database.History
import Database.Journal (Change (..), EditLine (..), JournalOp (..), ParameterLine (..), Prior (..), noteFrom)
import TestHelpers (modelDividingBy)
import Types (AllocationProperty (..), Parameter (..), ParameterDefinition (..), Reading (..), Release (..), StatedAmount (..))

bread :: AuthoredActivity
bread =
    AuthoredActivity
        { aaName = "bread"
        , aaLocation = "FR"
        , aaDescription = []
        , aaProductName = "bread"
        , aaProductAmount = 1
        , aaProductUnit = "kg"
        , aaExchanges = []
        }

flowId :: UUID.UUID
flowId = UUID.nil

value :: Double -> Parameter
value amount = Parameter{parameterDefinition = InputParameter amount Nothing, parameterUnit = Just "km", parameterDescription = ""}

history :: [HistoryLine]
history =
    describeJournal
        (namerFor Nothing)
        [ Entry "2026-10-01" (Change Nothing (Created [bread] ["a_b"]))
        , Entry "2026-10-02" (Change (noteFrom "Measured on site") (Edited "a_b" [EditLine (SetAmount (SelectBiosphere flowId) 3) 1 (Prior [1.2]), EditLine (RemoveExchange (SelectInput "c_d")) 1 PriorUnknown, EditLine (SetProperty Price flowId (Just (StatedAmount "EUR2005" 0.42))) 1 (Prior []), EditLine (SetProperty Price flowId Nothing) 1 (Prior [])]))
        , Entry "2026-10-03" (Change Nothing (ParametersEdited [ParameterLine "distance" (Just (value 90)) (Prior (Just (value 120))), ParameterLine "new" (Just (value 1)) (Prior Nothing)]))
        , Entry "2026-10-04" (Change Nothing (Deleted ["a_b"]))
        ]

spec :: Spec
spec = describe "Database.History" $ do
    it "says each change in words, with the value it replaced when it was kept" $
        map (\line -> (hlSummary line, hlDetails line)) history
            `shouldBe` [ ("Added 1 activity", ["\"bread\" (FR), a_b"])
                       , ("Edited \"a_b\"", ["Flow 00000000-0000-0000-0000-000000000000: amount from 1.2 set to 3.0", "Input from \"c_d\": removed", "Product 00000000-0000-0000-0000-000000000000: price set to 0.42 EUR2005", "Product 00000000-0000-0000-0000-000000000000: price removed"])
                       , ("Changed 2 database parameters", ["distance changed from 120.0 km to 90.0 km", "new added: 1.0 km"])
                       , ("Deleted 1 activity", ["\"a_b\""])
                       ]

    it "reports each change with its reason, or says it was given none" $ do
        let report = changesReport Provenance{pvDatabase = "my bakery", pvOriginal = "bakery", pvRelease = Just (Release "Bakery data" "2025" Nothing), pvReallocated = Just (ByModel (modelDividingBy DryMass))} (Carried Nothing history)
        T.lines report
            `shouldStartWith` [ "# Changes to my bakery"
                              , ""
                              , "Modified from bakery, Bakery data 2025."
                              , ""
                              , "Modified data; not endorsed by the publisher of the original."
                              , ""
                              , "Multi-output processes are divided by the system model every block by dry mass 1.0.0, not as the original divides them."
                              , ""
                              , "## 2026-10-01: Added 1 activity"
                              , ""
                              , "Reason: No reason given"
                              , ""
                              , "- \"bread\" (FR), a_b"
                              , ""
                              , "## 2026-10-02: Edited \"a_b\""
                              , ""
                              , "Reason: Measured on site"
                              ]
