{-# LANGUAGE OverloadedStrings #-}

{- | An activity's lines are packed: its plain biosphere lines in columns, the
others as they are. Packing must lose nothing, not a line and not their order,
since a line is also known by its position among the others.
-}
module PackedLinesSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Store (PeekException, decode, encode)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32, Word64)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Gen, choose, elements, frequency, listOf)

import PackedLines (Packable (..), PackedLines, keptLines, pack, unpack, unpackWith)
import Types

-- | Lines as a process mixes them: mostly plain, a few that say more.
newtype Lines = Lines [Exchange]
    deriving (Show)

instance Arbitrary Lines where
    arbitrary = Lines <$> listOf (frequency [(6, plain), (1, commented), (1, product')])
      where
        plain = BiosphereExchange <$> flow <*> amount <*> unit <*> elements [Resource, Emission] <*> place <*> pure Nothing <*> pure Nothing <*> pure Nothing
        commented = (\line -> line{bioComment = Just "measured on site"}) <$> plain
        product' = (\f n u -> TechnosphereExchange f n u ReferenceProduct Nothing ClaimByProduct LocationNone Nothing Nothing Nothing M.empty noProperties Nothing) <$> flow <*> amount <*> unit
        flow = UUID.fromWords64 <$> arbitrary <*> arbitrary
        amount = choose (-1.0e6, 1.0e6) :: Gen Double
        -- Few units and places, so that lines share a shape as real ones do.
        unit = UUID.fromWords64 0 <$> elements [1, 2]
        place = elements [LocationGlobal, LocationNone, LocationCode "FR"]

-- | The lines read from the bytes of packed lines, written part by part as their instance writes them.
bytesOf :: [Exchange] -> [(Word64, Word64, Double, Word32)] -> [BioShape] -> [Bool] -> Either PeekException [Exchange]
bytesOf kept rows shapes isPlain = unpack <$> (decode (encode (kept, VU.fromList rows, V.fromList shapes, VU.fromList isPlain)) :: Either PeekException (PackedLines Exchange))

spec :: Spec
spec = describe "the packed lines of an activity" $ do
    prop "come back as they were given, in order" $ \(Lines xs) ->
        unpack (pack xs) == xs

    prop "come back as they were given from the bytes of a cache" $ \(Lines xs) ->
        fmap unpack (decode (encode (pack xs)) :: Either PeekException (PackedLines Exchange)) == Right xs

    prop "keep as they are exactly the lines that are not plain" $ \(Lines xs) ->
        keptLines (pack xs) == filter (isNothing . toPlain) xs

    prop "give the flow of each line, in order, without building the line" $ \(Lines xs) ->
        unpackWith exchangeFlowId (\flow _ _ -> flow) (pack xs) == map exchangeFlowId xs

    it "are refused from bytes whose row names a shape that is not there" $
        bytesOf [] [(0, 0, 1, 5)] [BioShape UUID.nil Emission LocationGlobal] [True] `shouldSatisfy` isLeft

    it "are refused from bytes that count more plain lines than rows" $
        bytesOf [] [] [] [True] `shouldSatisfy` isLeft

    it "are refused from bytes that count more other lines than were kept" $
        bytesOf [] [] [] [False] `shouldSatisfy` isLeft

    it "are read from bytes that agree" $
        bytesOf [] [(0, 7, 2, 0)] [BioShape UUID.nil Emission LocationGlobal] [True]
            `shouldBe` Right [BiosphereExchange (UUID.fromWords64 0 7) 2 UUID.nil Emission LocationGlobal Nothing Nothing Nothing]
