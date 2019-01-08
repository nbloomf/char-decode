module Main where

import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import           Data.Typeable (typeOf, Typeable())
import           Data.Word (Word8)

import Data.Char.Decode

main :: IO ()
main =
  defaultMain $
    localOption (QC.QuickCheckTests 10000) $
      testGroup "Encodings"
        [ testEncoding CodePage437
        , testEncoding CodePage437Graphic
        ]

testEncoding
  :: (ByteEncoding enc, Typeable enc)
  => enc -> TestTree
testEncoding enc = testGroup (show $ typeOf enc)
  [ QC.testProperty "encode . decode" (_encode_decode enc)
  ]

_encode_decode
  :: (ByteEncoding enc)
  => enc -> Word8 -> Bool
_encode_decode enc x =
  (encodeByte enc $ decodeByte enc x) == (Right x)
