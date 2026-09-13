{-# LANGUAGE OverloadedStrings #-}

{- | Licenses & third-party attribution payload served at @/api/v1/licenses@.

CeCILL-C §6.4 requires that the bundled MUMPS notice be easily accessible
from the application interface. Apache-2.0 §4(d) requires that we propagate
attribution for every component we redistribute. This module exposes the
canonical inventory as a stable JSON shape so any client (web SPA, desktop,
MCP, pyvolca) can fetch and render it without duplicating the data.

The payload is a hand-written constant - small, no IO, no partial functions.
Keep it in sync with @NOTICE@ and @THIRD_PARTY_LICENSES.md@ at the repo root.
-}
module API.Licenses (licensesJson, licensesResponse) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Response, responseLBS)
import qualified Version

licensesJson :: BL.ByteString
licensesJson = encode licensesValue

{- | The fully-formed HTTP response served at @/api/v1/licenses@. Exported so
the dispatcher in @app\/Main.hs@ stays a one-liner and tests can assert on
status\/headers without spinning up a server.
-}
licensesResponse :: Response
licensesResponse =
    responseLBS
        status200
        [(hContentType, "application/json; charset=utf-8")]
        licensesJson

licensesValue :: Value
licensesValue =
    object
        [ "engine"
            .= object
                [ "name" .= ("VoLCA" :: Text)
                , "version" .= Version.version
                , "license" .= ("Apache-2.0" :: Text)
                , "copyright" .= ("Copyright (c) 2024-present Christophe Combelles and contributors" :: Text)
                , "homepage" .= ("https://www.volca.run/" :: Text)
                ]
        , "components" .= componentList
        , "haskell_dependencies_url" .= ("https://github.com/ccomb/volca/blob/main/THIRD_PARTY_LICENSES.md" :: Text)
        ]

componentList :: [Value]
componentList =
    [ component
        "MUMPS"
        (Just "5.8.1")
        "CeCILL-C"
        (Just "Copyright 1991-2024 CERFACS, CNRS, ENS Lyon, INP Toulouse, Inria, Mumps Technologies, University of Bordeaux.")
        (Just "https://mumps-solver.org/")
        Nothing
    , component
        "mumps-hs"
        Nothing
        "Apache-2.0"
        (Just "Copyright (c) 2024-present Christophe Combelles")
        (Just "https://github.com/ccomb/volca/tree/main/mumps-hs")
        Nothing
    , component
        "PORD (bundled in MUMPS)"
        Nothing
        "Public domain"
        (Just "Juergen Schulze")
        Nothing
        Nothing
    , component
        "AMD ordering and *_TRUNCATED_RRQR variants (bundled in MUMPS)"
        Nothing
        "BSD-3-Clause"
        Nothing
        Nothing
        Nothing
    , component
        "BLAS"
        Nothing
        "BSD-3-Clause"
        Nothing
        (Just "https://www.netlib.org/blas/")
        Nothing
    , component
        "LAPACK"
        Nothing
        "BSD-3-Clause"
        Nothing
        (Just "https://www.netlib.org/lapack/")
        Nothing
    ]

component :: Text -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Value
component name mVersion lic mCopyright mHomepage mSource =
    object
        [ "name" .= name
        , "version" .= mVersion
        , "license" .= lic
        , "copyright" .= mCopyright
        , "homepage" .= mHomepage
        , "source" .= mSource
        ]
