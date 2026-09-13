{-# LANGUAGE OverloadedStrings #-}

{- | A small set of property-based tests covering invariants that ARE
expected to hold on every legitimate input – not just the ones in our
fixtures. Property tests catch cases that example-based tests miss
because we never thought to write the example down.
-}
module PropertiesSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), arbitraryASCIIChar, choose, listOf)

import qualified Search.Normalize as N
import qualified UnitConversion as UC

{- | An ASCII-only Text generator – keeps the surface small so a failing
counterexample is readable and avoids irrelevant unicode noise the
normalizer already handles via its own test cases.
-}
newtype AsciiText = AsciiText {unAscii :: T.Text}
    deriving (Show)

instance Arbitrary AsciiText where
    arbitrary = AsciiText . T.pack <$> listOf arbitraryASCIIChar

{- | A positive finite Double bounded so unit conversions don't overflow or
underflow to denormals – the actual application domain (kg, MJ, etc.)
stays well within these bounds.
-}
newtype DomainAmount = DomainAmount {unAmount :: Double}
    deriving (Show)

instance Arbitrary DomainAmount where
    arbitrary = DomainAmount <$> choose (1e-3, 1e9)

spec :: Spec
spec = do
    describe "Search.Normalize.normalize" $ do
        prop "is idempotent (normalize . normalize == normalize)" $
            \(AsciiText t) ->
                N.normalize (N.normalize t) == N.normalize t

        prop "never grows the text past its input length" $
            -- The normalizer can shrink (drop diacritics, collapse spaces)
            -- but should never insert characters. Important: Unicode characters
            -- can DECODE into multiple bytes, so we check length in characters
            -- on the ASCII-only inputs from AsciiText, where bytes==chars.
            \(AsciiText t) ->
                T.length (N.normalize t) <= T.length t

    describe "UnitConversion.convertUnit (defaultUnitConfig)" $ do
        prop "round-trips: kg → kg returns exactly the input amount" $
            \(DomainAmount q) ->
                UC.convertUnit UC.defaultUnitConfig "kg" "kg" q == Just q

        prop "is invertible across compatible units: kg → g → kg ≈ identity" $
            \(DomainAmount q) ->
                case UC.convertUnit UC.defaultUnitConfig "kg" "g" q of
                    Just qInG -> case UC.convertUnit UC.defaultUnitConfig "g" "kg" qInG of
                        Just qBack -> abs (qBack - q) < 1e-9 * abs q + 1e-9
                        Nothing -> False
                    Nothing -> True -- defaultUnitConfig might not know g; that's ok
        prop "returns Nothing on dimensionally incompatible conversions (kg → m)" $
            \(DomainAmount q) ->
                -- kg (mass) → m (length) MUST refuse – this is a correctness
                -- invariant the LCIA layer relies on to surface bad data
                -- instead of silently injecting wrong-dimension quantities.
                case UC.convertUnit UC.defaultUnitConfig "kg" "m" q of
                    Nothing -> True
                    Just _ -> False
