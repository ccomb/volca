{-# LANGUAGE OverloadedStrings #-}

{- | Licenses & third-party attribution payload served at @/api/v1/licenses@.

Apache-2.0 §4(d) requires that we propagate attribution for every component
we redistribute, and the BSD and MIT licences of what is linked in ask for
their notices to travel with the binary. This module exposes the
canonical inventory as a stable JSON shape so any client (web SPA, desktop,
MCP, pyvolca) can fetch and render it without duplicating the data.

The payload is a hand-written constant – small, no IO, no partial functions.
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
        "GCC runtime (libgcc)"
        Nothing
        "GPL-3.0-or-later WITH GCC-exception-3.1"
        Nothing
        (Just "https://gcc.gnu.org/onlinedocs/gcc/Copying.html")
        Nothing
    , component
        "faer (dense linear algebra and ordering, used by the sparse solver)"
        Nothing
        "MIT"
        (Just "sarah quiñones")
        (Just "https://codeberg.org/sarah-quinones/faer")
        Nothing
    , component
        "AMD ordering (ported in faer)"
        Nothing
        "BSD-3-Clause"
        (Just "Copyright (c) 1996-2022, Timothy A. Davis, Patrick R. Amestoy, and Iain S. Duff")
        (Just "http://www.suitesparse.com")
        Nothing
    , component
        "Rust standard library"
        Nothing
        "MIT OR Apache-2.0"
        (Just "The Rust Project contributors")
        (Just "https://github.com/rust-lang/rust")
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
