{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

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
import GHC.Stack (HasCallStack)

import Data.ByteString (ByteString)

import Data.Text (Text)

import Control.Lens (
    Getter, Getting, Lens', Prism',
    (&), (^.), (.~), (?~),
    makeLenses, prism, review)

import Control.DeepSeq (NFData)

import Pinch (Enumeration, Field, Pinchable, enum, field, putField)

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
data Tag = Tag { _tagKey' :: !(Field 1 Text)
               , _tagVType' :: !(Field 2 TagType)
               , _tagVStr' :: !(Field 3 (Maybe Text))
               , _tagVDouble' :: !(Field 4 (Maybe Double))
               , _tagVBool' :: !(Field 5 (Maybe Bool))
               , _tagVLong':: !(Field 6 (Maybe Int64))
               , _tagVBinary' :: !(Field 7 (Maybe ByteString))
               }
    deriving (Show, Eq, Generic)

makeLenses ''Tag
instance NFData Tag
instance Pinchable Tag

-- | 'Tag' @key@.
tagKey :: Lens' Tag Text
tagKey = tagKey' . field
{-# INLINE tagKey #-}

-- | 'Tag' @vType@.
--
-- /Note:/ `tagType` is 'Getter', /not/ a 'Lens', otherwise it'd be too easy
-- to create invalid 'Tag's where the field corresponding with the 'TagType'
-- isn't set.
tagType :: Getter Tag TagType
tagType = tagVType' . field
{-# INLINE tagType #-}

fromTagUnion :: HasCallStack => Getting a (Maybe a) a
fromTagUnion f = maybe (error "Tag: mismatch between vType and optional fields") (fmap Just . f)
{-# INLINE fromTagUnion #-}

-- | Map a family of functions over the value carried in a 'Tag'.
mapTagValue :: (Text -> a)  -- ^ Handle a 'Text' payload
            -> (Double -> a)  -- ^ Handle a 'Double' payload
            -> (Bool -> a)  -- ^ Handle a 'Bool' payload
            -> (Int64 -> a)  -- ^ Handle an 'Int64' payload
            -> (ByteString -> a)  -- ^ Handle a 'ByteStrin' payload
            -> Tag  -- ^ 'Tag' whose value to map over
            -> a
mapTagValue t d b i b' tag = case tag ^. tagVType' . field of
    STRING _ -> t (tag ^. tagVStr' . field . fromTagUnion)
    DOUBLE _ -> d (tag ^. tagVDouble' . field . fromTagUnion)
    BOOL _ -> b (tag ^. tagVBool' . field . fromTagUnion)
    LONG _ -> i (tag ^. tagVLong' . field . fromTagUnion)
    BINARY _ -> b' (tag ^. tagVBinary' . field . fromTagUnion)

-- Note: This is a placeholder, but actually an invalid Tag.
emptyTag :: Tag
emptyTag = Tag { _tagKey' = putField mempty
               , _tagVType' = putField string
               , _tagVStr' = putField Nothing
               , _tagVDouble' = putField Nothing
               , _tagVBool' = putField Nothing
               , _tagVLong' = putField Nothing
               , _tagVBinary' = putField Nothing
               }
{-# INLINE emptyTag #-}

-- | 'Prism' for a 'Tag' carrying 'Text'.
_StringTag :: Prism' Tag (Text, Text)
_StringTag = prism wrap extract
  where
    wrap (k, v) = emptyTag & tagKey .~ k
                           & tagVType' . field .~ string
                           & tagVStr' . field ?~ v
    extract t = case t ^. tagVType' . field of
        STRING _ -> Right (t ^. tagKey, t ^. tagVStr' . field . fromTagUnion)
        _ -> Left t

-- | 'Prism' for a 'Tag' carrying a 'Double'.
_DoubleTag :: Prism' Tag (Text, Double)
_DoubleTag = prism wrap extract
  where
    wrap (k, v) = emptyTag & tagKey .~ k
                           & tagVType' . field .~ double
                           & tagVDouble' . field ?~ v
    extract t = case t ^. tagVType' . field of
        DOUBLE _ -> Right (t ^. tagKey, t ^. tagVDouble' . field . fromTagUnion)
        _ -> Left t

-- | 'Prism' for a 'Tag' carrying a 'Bool'.
_BoolTag :: Prism' Tag (Text, Bool)
_BoolTag = prism wrap extract
  where
    wrap (k, v) = emptyTag & tagKey .~ k
                           & tagVType' . field .~ bool
                           & tagVBool' . field ?~ v
    extract t = case t ^. tagVType' . field of
        BOOL _ -> Right (t ^. tagKey, t ^. tagVBool' . field . fromTagUnion)
        _ -> Left t

-- | 'Prism' for a 'Tag' carrying an 'Int64'.
_LongTag :: Prism' Tag (Text, Int64)
_LongTag = prism wrap extract
  where
    wrap (k, v) = emptyTag & tagKey .~ k
                           & tagVType' . field .~ long
                           & tagVLong' . field ?~ v
    extract t = case t ^. tagVType' . field of
        LONG _ -> Right (t ^. tagKey, t ^. tagVLong' . field . fromTagUnion)
        _ -> Left t

-- | 'Prism' for a 'Tag' carrying a 'ByteString'.
_BinaryTag :: Prism' Tag (Text, ByteString)
_BinaryTag = prism wrap extract
  where
    wrap (k, v) = emptyTag & tagKey .~ k
                           & tagVType' . field .~ binary
                           & tagVBinary' . field ?~ v
    extract t = case t ^. tagVType' . field of
        BINARY _ -> Right (t ^. tagKey, t ^. tagVBinary' . field . fromTagUnion)
        _ -> Left t


-- | Construct a 'Text' 'Tag'.
stringTag :: Text -> Text -> Tag
stringTag = curry (review _StringTag)
{-# INLINE stringTag #-}

-- | Construct a 'Double' 'Tag'.
doubleTag :: Text -> Double -> Tag
doubleTag = curry (review _DoubleTag)
{-# INLINE doubleTag #-}

-- | Construct a 'Bool' 'Tag'.
boolTag :: Text -> Bool -> Tag
boolTag = curry (review _BoolTag)
{-# INLINE boolTag #-}

-- | Construct an 'Int64' 'Tag'.
longTag :: Text -> Int64 -> Tag
longTag = curry (review _LongTag)
{-# INLINE longTag #-}

-- | Construct a 'ByteString' 'Tag'.
binaryTag :: Text -> ByteString -> Tag
binaryTag = curry (review _BinaryTag)
{-# INLINE binaryTag #-}
