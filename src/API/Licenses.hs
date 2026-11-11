{-# LANGUAGE OverloadedStrings #-}

{- | Licenses & third-party attribution payload served at @/api/v1/licenses@.

Apache-2.0 §4(d) requires that we propagate attribution for every component
we redistribute. This module exposes the canonical inventory as a stable JSON
shape so any client (web SPA, desktop, MCP, pyvolca) can fetch and render it
without duplicating the data.

The payload is a hand-written constant – small, no IO, no partial functions.
Keep it in sync with @NOTICE@ and @THIRD_PARTY_LICENSES.md@ at the repo root.
-}
module API.Licenses (licensesJson, licensesResponse) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Response, responseLBS)
import System.Info (os)
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
        , "components" .= map component (filter ((os `elem`) . nativePlatforms) nativeLibraries)
        , "haskell_dependencies_url" .= ("https://github.com/ccomb/volca/blob/main/THIRD_PARTY_LICENSES.md" :: Text)
        ]

-- | A C library linked into the binary, on the platforms named as 'os' names them.
data NativeLibrary = NativeLibrary
    { nativeName :: Text
    , nativeLicense :: Text
    , nativeCopyright :: Text
    , nativeHomepage :: Text
    , nativePlatforms :: [String]
    }

nativeLibraries :: [NativeLibrary]
nativeLibraries =
    [ NativeLibrary "GMP (GNU MP)" "LGPL-3.0-or-later" "Copyright 1991-2023 Free Software Foundation, Inc." "https://gmplib.org/" everywhere
    , NativeLibrary "Zstandard" "BSD-3-Clause" "Copyright (c) Meta Platforms, Inc. and affiliates." "https://facebook.github.io/zstd/" everywhere
    , NativeLibrary "zlib" "Zlib" "Copyright (C) 1995-2026 Jean-loup Gailly and Mark Adler" "https://zlib.net/" [linux, windows]
    , NativeLibrary "musl" "MIT" "Copyright (c) 2005-2020 Rich Felker, et al." "https://musl.libc.org/" [linux]
    , NativeLibrary "libffi" "MIT" "Copyright (c) 1996-2025 Anthony Green, Red Hat, Inc and others." "https://sourceware.org/libffi/" [linux]
    , NativeLibrary "ncurses" "X11" "Copyright 2018-2024 Thomas E. Dickey, Copyright 1998-2018 Free Software Foundation, Inc." "https://invisible-island.net/ncurses/" [linux]
    , NativeLibrary "MinGW-w64 runtime" "ZPL-2.1 and others" "Copyright (c) 2009-2013 by the mingw-w64 project" "https://www.mingw-w64.org/" [windows]
    ]
  where
    -- macOS loads zlib, libffi and ncurses from the system: they are not distributed.
    everywhere = [linux, "darwin", windows]
    linux = "linux"
    windows = "mingw32"

-- | The shape clients decode, which keeps a version and a source field it leaves empty.
component :: NativeLibrary -> Value
component library =
    object
        [ "name" .= nativeName library
        , "version" .= (Nothing :: Maybe Text)
        , "license" .= nativeLicense library
        , "copyright" .= nativeCopyright library
        , "homepage" .= nativeHomepage library
        , "source" .= (Nothing :: Maybe Text)
        ]
