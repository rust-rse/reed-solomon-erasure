module Vector (tests) where

import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Vector.Storable.ByteString (fromByteString, toByteString)

vectorByteString :: [Word8] -> Bool
vectorByteString l = toByteString (V.fromList l) == B.pack l

byteStringVector :: [Word8] -> Bool
byteStringVector l = fromByteString (B.pack l) == V.fromList l

roundtripVectorByteString :: [Word8] -> Bool
roundtripVectorByteString l = V.toList (fromByteString $ toByteString $ V.fromList l) == l

roundtripByteStringVector :: [Word8] -> Bool
roundtripByteStringVector l = B.unpack (toByteString $ fromByteString $ B.pack l) == l

tests :: TestTree
tests = testGroup "Vector" [
      testGroup "Properties" [
            testProperty "vectorByteString" vectorByteString
          , testProperty "byteStringVector" byteStringVector
          , testProperty "roundtripVectorByteString" roundtripVectorByteString
          , testProperty "roundtripByteStringVector" roundtripByteStringVector
          ]
    ]
