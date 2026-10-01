{-# LANGUAGE OverloadedStrings #-}

{- | How each format's dates are read: the ISO timestamps of the XML formats,
and the date a SimaPro process states in the format its header declares.
-}
module DatasetDatesSpec (spec) where

import Data.Either (isLeft)
import Data.Time.Calendar (fromGregorian)
import SimaPro.Parser (DateNote (..), SimaProConfig (..), StatedDate (..), dateWarnings, defaultConfig, readStatedDate)
import Test.Hspec
import Types (readIsoDate)

spec :: Spec
spec = do
    describe "readIsoDate" $ do
        it "keeps the day of a dateTime" $
            readIsoDate "2011-06-30T13:32:41" `shouldBe` Right (Just (fromGregorian 2011 6 30))
        it "reads a bare date" $
            readIsoDate " 2024-09-06 " `shouldBe` Right (Just (fromGregorian 2024 9 6))
        it "reads a blank value as no date" $
            readIsoDate "  " `shouldBe` Right Nothing
        it "refuses a date written another way" $
            readIsoDate "30/06/2011" `shouldSatisfy` isLeft

    describe "readStatedDate" $ do
        let dotted = defaultConfig{spDateFormat = "dd/MM/yyyy", spDateSeparator = '.'}
        it "reads the slash of the format as the declared separator" $
            readStatedDate dotted "15.01.2016" `shouldBe` DateStated (fromGregorian 2016 1 15)
        it "reads the default slash-separated format" $
            readStatedDate defaultConfig "02/08/2011" `shouldBe` DateStated (fromGregorian 2011 8 2)
        it "reads a day the format pads but the file does not" $
            readStatedDate defaultConfig "2/8/2011" `shouldBe` DateStated (fromGregorian 2011 8 2)
        it "reads the calendar's day zero as no date" $
            readStatedDate dotted "30.12.1899" `shouldBe` DateZero
        it "reads a blank line as no date" $
            readStatedDate dotted "" `shouldBe` DateBlank
        it "refuses a date written with another separator" $
            readStatedDate dotted "15/01/2016" `shouldSatisfy` unreadable
        it "refuses a day that does not exist" $
            readStatedDate defaultConfig "31/02/2016" `shouldSatisfy` unreadable
        it "refuses 30/12/99 under a two-digit year, which reads two ways" $
            readStatedDate defaultConfig{spDateFormat = "dd/MM/yy"} "30/12/99" `shouldBe` DateZeroOrDay
        it "refuses a two-digit year under a four-digit format" $
            readStatedDate defaultConfig "15/01/16" `shouldSatisfy` unreadable
        it "reads any other day under a two-digit year" $
            readStatedDate defaultConfig{spDateFormat = "dd/MM/yy"} "15/01/16" `shouldBe` DateStated (fromGregorian 2016 1 15)
        it "refuses a format it does not know rather than guessing" $
            readStatedDate defaultConfig{spDateFormat = "MMM d, yyyy"} "Jan 5, 2016" `shouldSatisfy` unreadable

    describe "dateWarnings" $
        it "names each unreadable date and counts the zeros once" $
            dateWarnings [DateNote "bread" (DateUnreadable "bad"), DateNote "flour" DateZero, DateNote "salt" DateZero, DateNote "oil" DateZeroOrDay]
                `shouldBe` [ "process 'bread': Date bad; the process is read with no date"
                           , "2 processes write the date SimaPro writes for none (30 December 1899): read as no date"
                           , "1 processes write 30/12/99 under a two-digit year, either the date SimaPro writes for none or 30 December 1999: read as no date"
                           ]
  where
    unreadable :: StatedDate -> Bool
    unreadable (DateUnreadable _) = True
    unreadable (DateStated _) = False
    unreadable DateBlank = False
    unreadable DateZero = False
    unreadable DateZeroOrDay = False
