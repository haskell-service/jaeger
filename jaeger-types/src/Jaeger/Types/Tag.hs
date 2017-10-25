{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Jaeger.Types.Tag (
      Tag
    , stringTag, doubleTag, boolTag, longTag, binaryTag
    , tagKey, tagType
    , _StringTag, _DoubleTag, _BoolTag, _LongTag, _BinaryTag
    , mapTagValue
    , TagType, string, double, bool, long, binary
    ) where

import Data.Int (Int64)
import GHC.Generics (Generic)

import Data.ByteString (ByteString)

import Data.Text (Text)

import Control.Lens (Getter, Lens', lens, makePrisms, to)

import Control.DeepSeq (NFData)

import Pinch (Enumeration, Pinchable, TStruct, (.=), (.:), enum, struct)
import qualified Pinch

-- | 'TagType' denotes the type of a 'Tag's value.
--
-- > enum TagType { STRING, DOUBLE, BOOL, LONG, BINARY }
data TagType = STRING (Enumeration 0)
             | DOUBLE (Enumeration 1)
             | BOOL (Enumeration 2)
             | LONG (Enumeration 3)
             | BINARY (Enumeration 4)
    deriving (Show, Eq, Generic)

instance NFData TagType
instance Pinchable TagType

-- | 'Tag' carries a 'Text' value.
string :: TagType
string = STRING enum
-- | 'Tag' carries a 'Double' value.
double :: TagType
double = DOUBLE enum
-- | 'Tag' carries a 'Bool' value.
bool :: TagType
bool = BOOL enum
-- | 'Tag' carries an 'Int64' value.
long :: TagType
long = LONG enum
-- | 'Tag' carries a 'ByteString' value.
binary :: TagType
binary = BINARY enum


-- | 'Tag' is a basic strongly typed key/value pair.
--
-- > struct Tag {
-- >   1: required string  key
-- >   2: required TagType vType
-- >   3: optional string  vStr
-- >   4: optional double  vDouble
-- >   5: optional bool    vBool
-- >   6: optional i64     vLong
-- >   7: optional binary  vBinary
-- > }
data Tag = StringTag !Text !Text
         | DoubleTag !Text !Double
         | BoolTag !Text !Bool
         | LongTag !Text !Int64
         | BinaryTag !Text !ByteString
    deriving (Show, Eq, Generic)

makePrisms ''Tag
instance NFData Tag

instance Pinch.Pinchable Tag where
    type Tag Tag = TStruct

    pinch = \case
        StringTag k v -> struct [ 1 .= k
                                , 2 .= string
                                , 3 .= v
                                ]
        DoubleTag k v -> struct [ 1 .= k
                                , 2 .= double
                                , 4 .= v
                                ]
        BoolTag k v -> struct [ 1 .= k
                              , 2 .= bool
                              , 5 .= v
                              ]
        LongTag k v -> struct [ 1 .= k
                              , 2 .= long
                              , 6 .= v
                              ]
        BinaryTag k v -> struct [ 1 .= k
                                , 2 .= binary
                                , 7 .= v
                                ]
    unpinch value = do
        typ <- value .: 2
        case typ of
            STRING _ -> StringTag <$> value .: 1 <*> value .: 3
            DOUBLE _ -> DoubleTag <$> value .: 1 <*> value .: 4
            BOOL _ -> BoolTag <$> value .: 1 <*> value .: 5
            LONG _ -> LongTag <$> value .: 1 <*> value .: 6
            BINARY _ -> BinaryTag <$> value .: 1 <*> value .: 7

-- | 'Tag' @key@.
tagKey :: Lens' Tag Text
tagKey = lens get set
  where
    get = \case
        StringTag k _ -> k
        DoubleTag k _ -> k
        BoolTag k _ -> k
        LongTag k _ -> k
        BinaryTag k _ -> k
    set tag k = case tag of
        StringTag _ v -> StringTag k v
        DoubleTag _ v -> DoubleTag k v
        BoolTag _ v -> BoolTag k v
        LongTag _ v -> LongTag k v
        BinaryTag _ v -> BinaryTag k v
{-# INLINE tagKey #-}

-- | 'Tag' @vType@.
tagType :: Getter Tag TagType
tagType = to $ \case
    StringTag{} -> string
    DoubleTag{} -> double
    BoolTag{} -> bool
    LongTag{} -> long
    BinaryTag{} -> binary
{-# INLINE tagType #-}

-- | Map a family of functions over the value carried in a 'Tag'.
mapTagValue :: (Text -> a)  -- ^ Handle a 'Text' payload
            -> (Double -> a)  -- ^ Handle a 'Double' payload
            -> (Bool -> a)  -- ^ Handle a 'Bool' payload
            -> (Int64 -> a)  -- ^ Handle an 'Int64' payload
            -> (ByteString -> a)  -- ^ Handle a 'ByteStrin' payload
            -> Tag  -- ^ 'Tag' whose value to map over
            -> a
mapTagValue t d b i b' = \case
    StringTag _ v -> t v
    DoubleTag _ v -> d v
    BoolTag _ v -> b v
    LongTag _ v -> i v
    BinaryTag _ v -> b' v

-- | Construct a 'Text' 'Tag'.
stringTag :: Text -> Text -> Tag
stringTag = StringTag
{-# INLINE stringTag #-}

-- | Construct a 'Double' 'Tag'.
doubleTag :: Text -> Double -> Tag
doubleTag = DoubleTag
{-# INLINE doubleTag #-}

-- | Construct a 'Bool' 'Tag'.
boolTag :: Text -> Bool -> Tag
boolTag = BoolTag
{-# INLINE boolTag #-}

-- | Construct an 'Int64' 'Tag'.
longTag :: Text -> Int64 -> Tag
longTag = LongTag
{-# INLINE longTag #-}

-- | Construct a 'ByteString' 'Tag'.
binaryTag :: Text -> ByteString -> Tag
binaryTag = BinaryTag
{-# INLINE binaryTag #-}
