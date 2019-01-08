{-|
Module      : Text.Char.Decode
Description : Convert between legacy byte encodings and UTF-8.
Copyright   : (c) Automattic, Inc. 2018
License     : BSD3
Maintainer  : Nathan Bloomfield, nathan.bloomfield@a8c.com
Stability   : experimental
Portability : POSIX

Simple utilities for converting between legacy byte encodings and Unicode.
-}

module Data.Char.Decode
  ( -- * Class
    ByteEncoding(..)

    -- ** Decoding
  , decodeToText
  , decodeToString

    -- ** Encoding
  , encodeWithDefault
  , encodeFromText
  , encodeFromTextWithDefault
  , encodeFromString
  , encodeFromStringWithDefault

    -- * Character Mappings
  , CodePage437(..)
  , CodePage437Graphic(..)

    -- * Utilities
  , byte0
  , byte1
  , byte2
  , byte3
  ) where

import Data.Char.Decode.Class
import Data.Char.Decode.CodePage437
import Data.Char.Decode.CodePage437.Graphic
