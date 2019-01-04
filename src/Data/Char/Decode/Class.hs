module Data.Char.Decode.Class
  ( ByteEncoding(..)
  , encodeWithDefault
  , decodeToText
  , encodeFromText
  , encodeFromTextWithDefault
  , decodeToString
  , encodeFromString
  , encodeFromStringWithDefault
  , byte0
  , byte1
  , byte2
  , byte3
  ) where

import           Control.Monad (foldM)
import qualified Data.ByteString.Lazy as BS
import           Data.Char (ord)
import qualified Data.Text as T
import           Data.Word (Word8)

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

-- Low byte
lo k = rem k 256

-- High bytes
hi k = div (k - lo k) 256

byte0 :: Char -> Word8
byte0 = fromIntegral . lo . ord

byte1 :: Char -> Word8
byte1 = fromIntegral . lo . hi . ord

byte2 :: Char -> Word8
byte2 = fromIntegral . lo . hi . hi . ord

byte3 :: Char -> Word8
byte3 = fromIntegral . lo . hi . hi . hi . ord
