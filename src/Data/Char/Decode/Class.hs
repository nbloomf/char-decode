module Data.Char.Decode.Class
  ( ByteEncoding(..)
  , encodeWithDefault
  , decodeToText
  , encodeFromText
  , encodeFromTextWithDefault
  , decodeToString
  , encodeFromString
  , encodeFromStringWithDefault
  ) where

import           Control.Monad (foldM)
import           Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS

class ByteEncoding enc where
  decodeByte :: enc -> Word8 -> Char

  encodeByte :: enc -> Char -> Either Char Word8

encodeWithDefault
  :: (ByteEncoding enc)
  => enc -> Word8 -> Char -> Word8
encodeWithDefault enc def c =
  case encodeByte enc c of
    Left _   -> def
    Right pt -> pt

decodeToText
  :: (ByteEncoding enc)
  => enc -> BS.ByteString -> T.Text
decodeToText enc =
  T.pack . map (decodeByte enc) . BS.unpack

decodeToString
  :: (ByteEncoding enc)
  => enc -> BS.ByteString -> String
decodeToString enc =
  map (decodeByte enc) . BS.unpack

encodeFromText
  :: (ByteEncoding enc)
  => enc -> T.Text -> Either Char BS.ByteString
encodeFromText enc =
  fmap BS.pack . mapM (encodeByte enc) . T.unpack

encodeFromString
  :: (ByteEncoding enc)
  => enc -> String -> Either Char BS.ByteString
encodeFromString enc =
  fmap BS.pack . mapM (encodeByte enc)

encodeFromTextWithDefault
  :: (ByteEncoding enc)
  => enc -> Word8 -> T.Text -> BS.ByteString
encodeFromTextWithDefault enc def =
  BS.pack . map (encodeWithDefault enc def) . T.unpack

encodeFromStringWithDefault
  :: (ByteEncoding enc)
  => enc -> Word8 -> String -> BS.ByteString
encodeFromStringWithDefault enc def =
  BS.pack . map (encodeWithDefault enc def)
