{- |
  This module provides serialization primitives whose 
  endian is tied to the Encoder rather than the data 
  structures themselves.
-}
module Encode where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as B
import           Data.Int                (Int16, Int32, Int64, Int8)
import           Data.Word               (Word16, Word32, Word64, Word8)
import           GHC.Generics



-- | Little endian encoder
newtype EncodeLE = EncodeLE { encodeLE :: B.Builder }
  deriving newtype (Semigroup, Monoid)

-- | Big-endian encoder
newtype EncodeBE = EncodeBE { encodeBE :: B.Builder }
  deriving newtype (Semigroup, Monoid)

class Monoid s => Encode s where
  bytes :: B.ByteString -> s

  u8  :: Word8  -> s
  u16 :: Word16 -> s
  u32 :: Word32 -> s
  u64 :: Word64 -> s

  i8  :: Int8  -> s
  i16 :: Int16 -> s
  i32 :: Int32 -> s
  i64 :: Int64 -> s

instance Encode EncodeLE where
  bytes = EncodeLE . B.byteString

  u8  = EncodeLE . B.word8
  u16 = EncodeLE . B.word16LE
  u32 = EncodeLE . B.word32LE
  u64 = EncodeLE . B.word64LE

  i8  = EncodeLE . B.int8
  i16 = EncodeLE . B.int16LE
  i32 = EncodeLE . B.int32LE
  i64 = EncodeLE . B.int64LE

instance Encode EncodeBE where
  bytes = EncodeBE . B.byteString

  u8  = EncodeBE . B.word8
  u16 = EncodeBE . B.word16BE
  u32 = EncodeBE . B.word32BE
  u64 = EncodeBE . B.word64BE

  i8  = EncodeBE . B.int8
  i16 = EncodeBE . B.int16BE
  i32 = EncodeBE . B.int32BE
  i64 = EncodeBE . B.int64BE



class Put a where
  put :: Encode s => a -> s

  default put :: (Generic a, GPut (Rep a), Encode s) => a -> s
  put = gput . from


class GPut f where
  gput :: Encode s => f a -> s

instance Put a => GPut (K1 i a) where
  gput (K1 x) = put x

instance GPut a => GPut (M1 i c a) where
  gput (M1 x) = gput x

instance (GPut a, GPut b) => GPut (a :*: b) where
  gput (x :*: y) = gput x <> gput y



instance Put B.ByteString where put = bytes

instance Put Word8  where put = u8
instance Put Word16 where put = u16
instance Put Word32 where put = u32
instance Put Word64 where put = u64

instance Put Int8  where put = i8
instance Put Int16 where put = i16
instance Put Int32 where put = i32
instance Put Int64 where put = i64



enum :: (Enum a, Integral b) => a -> b
enum = fromIntegral . fromEnum

-- * Enum serialization 
{- $enumSerialization

  'U8', 'U16', 'U32', 'U64' can be used to derive 'Put' for Enums
  as follows. 

  The size of 'fromEnum' is not checked however, so 
  it may result in over/underflow.

  @
  data Color = Red | Green | Blue
    deriving Enum
    deriving Put via U32 Color -- serialize Color as a Word32
  @
-}

newtype U8 a  = U8 a
newtype U16 a = U16 a
newtype U32 a = U32 a
newtype U64 a = U64 a

instance Enum a => Put (U8  a) where put (U8 x)  = u8  $ enum x
instance Enum a => Put (U16 a) where put (U16 x) = u16 $ enum x
instance Enum a => Put (U32 a) where put (U32 x) = u32 $ enum x
instance Enum a => Put (U64 a) where put (U64 x) = u64 $ enum x
