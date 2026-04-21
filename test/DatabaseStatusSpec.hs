{-# LANGUAGE OverloadedStrings #-}

module DatabaseStatusSpec (spec) where

import Data.Aeson (decode, encode)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromJust)
import Test.Hspec

import API.DatabaseHandlers (convertDbStatus)
import API.Types (DatabaseStatusAPI (..))
import Database.Manager (DatabaseLoadStatus (..), DatabaseStatus (..))

mkStatus :: [A.Value -> A.Value] -> DatabaseStatus
mkStatus _ =
    DatabaseStatus
        { dsName = "agribalyse-3-2"
        , dsDisplayName = "Agribalyse 3.2"
        , dsDescription = Nothing
        , dsLoadAtStartup = True
        , dsStatus = Loaded
        , dsIsUploaded = False
        , dsPath = "data/agribalyse"
        , dsFormat = Nothing
        , dsActivityCount = 42
        , dsDependsOn = ["ecoinvent-3-9-1-adapted", "wfldb"]
        }

spec :: Spec
spec = do
    describe "DatabaseStatus with dsDependsOn" $ do
        it "round-trips through JSON" $ do
            let ds = mkStatus []
                bs = encode ds
                decoded = decode bs :: Maybe DatabaseStatus
            decoded `shouldBe` Just ds

        it "defaults dsDependsOn to [] for payloads written before the field existed" $ do
            let legacy =
                    A.object
                        [ "dsName" A..= ("x" :: String)
                        , "dsDisplayName" A..= ("X" :: String)
                        , "dsLoadAtStartup" A..= True
                        , "dsStatus" A..= Loaded
                        , "dsIsUploaded" A..= False
                        , "dsPath" A..= ("p" :: String)
                        , "dsActivityCount" A..= (0 :: Int)
                        ]
            let decoded = A.fromJSON legacy :: A.Result DatabaseStatus
            case decoded of
                A.Success ds -> dsDependsOn ds `shouldBe` []
                A.Error e -> expectationFailure e

    describe "convertDbStatus" $ do
        it "copies dsDependsOn into dsaDependsOn" $ do
            let api = convertDbStatus (mkStatus [])
            dsaDependsOn api `shouldBe` ["ecoinvent-3-9-1-adapted", "wfldb"]

    describe "DatabaseStatusAPI wire shape" $ do
        it "emits a 'dependsOn' JSON key (stripped-prefix convention)" $ do
            let api = convertDbStatus (mkStatus [])
            case A.toJSON api of
                A.Object o ->
                    KM.lookup "dependsOn" o
                        `shouldBe` Just (A.toJSON ["ecoinvent-3-9-1-adapted" :: String, "wfldb"])
                other -> expectationFailure ("expected object, got: " <> show other)

        it "round-trips through the API envelope" $ do
            let api = convertDbStatus (mkStatus [])
                roundTripped = fromJust (decode (encode api)) :: DatabaseStatusAPI
            dsaDependsOn roundTripped `shouldBe` dsaDependsOn api
