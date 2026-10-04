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
import TestHelpers (membersOnly)
import Types (AllocationKey (..), AllocationProperty (..), Licence (..), Release (..))

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
        , dsAllocation = ByProperty WetMass
        , dsSource = Just "agribalyse-3-2-declared"
        , dsLicence = membersOnly
        , dsRelease = Just (Release "Agribalyse" "3.2" Nothing)
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
                A.Success ds -> do
                    dsDependsOn ds `shouldBe` []
                    dsRelease ds `shouldBe` Nothing
                    -- Written before a database could be re-keyed, so it was
                    -- divided the way its source declares.
                    dsAllocation ds `shouldBe` Declared
                    dsSource ds `shouldBe` Nothing
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

        it "says which key divided it, and whose files it reads" $ do
            let api = convertDbStatus (mkStatus [])
            dsaAllocation api `shouldBe` "wet mass"
            dsaSource api `shouldBe` Just "agribalyse-3-2-declared"

        it "says the licence it is served under" $
            dsaLicence (convertDbStatus (mkStatus [])) `shouldBe` membersOnly

        it "reads a status written before the licence was on the wire as having none" $
            case A.toJSON (mkStatus []) of
                A.Object o -> (dsLicence <$> A.decode (encode (KM.delete "dsLicence" o))) `shouldBe` Just LicenceUnstated
                other -> expectationFailure ("expected object, got: " <> show other)
