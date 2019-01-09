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

-- | Represents encodings of bytes as characters. It's not expected that you will need to implement this class directly.
class ByteEncoding enc where
  -- | Mapping from bytes to chars.
  decodeByte :: enc -> Word8 -> Char

  -- | Mapping from chars to bytes. If the character @c@ has no analog in the encoding, returns @Left c@.
  encodeByte :: enc -> Char -> Either Char Word8

-- | If the character has no analog in the encoding, returns the default byte.
encodeWithDefault
  :: (ByteEncoding enc)
  => enc -> Word8 -> Char -> Word8
encodeWithDefault enc def c =
  case encodeByte enc c of
    Left _   -> def
    Right pt -> pt

-- | Convert a legacy encoded @ByteString@ to a UTF-8 encoded @Text@.
decodeToText
  :: (ByteEncoding enc)
  => enc -> BS.ByteString -> T.Text
decodeToText enc =
  T.pack . map (decodeByte enc) . BS.unpack

-- | Convert a legacy encoded @ByteString@ to a UTF-8 encoded @String@.
decodeToString
  :: (ByteEncoding enc)
  => enc -> BS.ByteString -> String
decodeToString enc =
  map (decodeByte enc) . BS.unpack

-- | If any input characters are out of range, returns @Left c@ where @c@ is the first such character.
encodeFromText
  :: (ByteEncoding enc)
  => enc -> T.Text -> Either Char BS.ByteString
encodeFromText enc =
  fmap BS.pack . mapM (encodeByte enc) . T.unpack

-- | If any input characters are out of range, returns @Left c@ where @c@ is the first such character.
encodeFromString
  :: (ByteEncoding enc)
  => enc -> String -> Either Char BS.ByteString
encodeFromString enc =
  fmap BS.pack . mapM (encodeByte enc)

-- | If any input characters are out of range, replace them with the given default.
encodeFromTextWithDefault
  :: (ByteEncoding enc)
  => enc -> Word8 -> T.Text -> BS.ByteString
encodeFromTextWithDefault enc def =
  BS.pack . map (encodeWithDefault enc def) . T.unpack

-- | If any input characters are out of range, replace them with the given default.
encodeFromStringWithDefault
  :: (ByteEncoding enc)
  => enc -> Word8 -> String -> BS.ByteString
encodeFromStringWithDefault enc def =
  BS.pack . map (encodeWithDefault enc def)

-- Low byte
lo k = rem k 256

-- High bytes
hi k = div (k - lo k) 256

-- | Extract the least significant byte (byte 0) of a UTF-8 code point.
byte0 :: Char -> Word8
byte0 = fromIntegral . lo . ord

-- | Extract the next-to-least significant byte (byte 1) of a UTF-8 code point.
byte1 :: Char -> Word8
byte1 = fromIntegral . lo . hi . ord

-- | Extract byte 2 of a UTF-8 code point.
byte2 :: Char -> Word8
byte2 = fromIntegral . lo . hi . hi . ord

-- | Extract byte 3 of a UTF-8 code point.
byte3 :: Char -> Word8
byte3 = fromIntegral . lo . hi . hi . hi . ord
