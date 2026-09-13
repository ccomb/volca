{-# LANGUAGE OverloadedStrings #-}

module MinimalCoverSpec (spec) where

import Data.Text (Text)
import qualified Data.UUID as UUID
import Test.Hspec
import Types (CrossDBLink (..), computeMinimalSelectedDeps, crossDBRedundantSources)

spec :: Spec
spec = do
    describe "computeMinimalSelectedDeps" $ do
        it "returns [] for no cross-DB links" $
            computeMinimalSelectedDeps [] `shouldBe` []

        it "keeps a single contributing DB" $
            computeMinimalSelectedDeps [mkLink "agribalyse-3-2" []]
                `shouldBe` ["agribalyse-3-2"]

        it "keeps a DB that is the only supplier for at least one link" $ do
            -- A wins one link with no alternative → A is essential. B wins
            -- another link with A as a tied alternative → already covered.
            let links =
                    [ mkLink "agribalyse-3-2" []
                    , mkLink "wfldb" ["agribalyse-3-2"]
                    ]
            computeMinimalSelectedDeps links `shouldBe` ["agribalyse-3-2"]

        it "drops a fully-redundant DB (PastoEco-shaped case)" $ do
            -- 168 links resolved uniquely by agribalyse, 1 link tied between
            -- wfldb (winner) and agribalyse → wfldb is redundant.
            let agriLinks = replicate 168 (mkLink "agribalyse-3-2" [])
                tiedLink = mkLink "wfldb" ["agribalyse-3-2"]
            computeMinimalSelectedDeps (agriLinks ++ [tiedLink])
                `shouldBe` ["agribalyse-3-2"]

        it "picks one representative deterministically when DBs only cover each other (D1 trap)" $ do
            -- Every link has tied set {A, B}, neither is uniquely essential.
            -- Greedy alphabetical pick → A only, not both.
            let links =
                    [ mkLink "alpha" ["beta"]
                    , mkLink "beta" ["alpha"]
                    , mkLink "alpha" ["beta"]
                    ]
            computeMinimalSelectedDeps links `shouldBe` ["alpha"]

        it "keeps both DBs when each is essential for at least one link" $ do
            let links =
                    [ mkLink "agribalyse-3-2" [] -- only agribalyse covers this
                    , mkLink "wfldb" [] -- only wfldb covers this
                    ]
            computeMinimalSelectedDeps links `shouldBe` ["agribalyse-3-2", "wfldb"]

        it "expands beyond essentials when a tied link uses none of them" $ do
            -- A is essential. A separate link is tied between {C, D} – neither
            -- A nor any essential covers it, so pick C alphabetically.
            let links =
                    [ mkLink "agribalyse-3-2" []
                    , mkLink "ginko" ["wfldb"] -- tied between ginko and wfldb
                    ]
            computeMinimalSelectedDeps links `shouldBe` ["agribalyse-3-2", "ginko"]

    describe "crossDBRedundantSources" $ do
        it "returns winners that are not in the selected set" $ do
            let links =
                    [ mkLink "agribalyse-3-2" []
                    , mkLink "wfldb" ["agribalyse-3-2"]
                    ]
                selected = ["agribalyse-3-2"]
            crossDBRedundantSources links selected `shouldBe` ["wfldb"]

        it "is empty when every winner is selected" $ do
            let links = [mkLink "agribalyse-3-2" []]
            crossDBRedundantSources links ["agribalyse-3-2"] `shouldBe` []

{- | Build a minimal CrossDBLink whose only meaningful fields here are
cdlSourceDatabase and cdlTiedAlternatives.
-}
mkLink :: Text -> [Text] -> CrossDBLink
mkLink src tied =
    CrossDBLink
        { cdlConsumerActUUID = UUID.nil
        , cdlConsumerProdUUID = UUID.nil
        , cdlConsumerFlowId = UUID.nil
        , cdlSupplierActUUID = UUID.nil
        , cdlSupplierProdUUID = UUID.nil
        , cdlCoefficient = 0
        , cdlExchangeUnit = ""
        , cdlFlowName = ""
        , cdlLocation = ""
        , cdlSourceDatabase = src
        , cdlTiedAlternatives = tied
        }
