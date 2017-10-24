{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

module Main (main) where

import Prelude hiding (log, span)

import Data.Int (Int32)

import GHC.Generics (Generic)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import qualified Data.Set as Set

import Control.Lens hiding (elements, enum)
import Control.Lens.Properties (isIso, isLens, isPrism)

import Pinch (
    Enumeration, Field, Pinchable,
    binaryProtocol, compactProtocol, decode, decodeMessage, encode,
    encodeMessage, enum, getField, pinch, putField, unpinch, runParser)

import Thrift.Protocol.Binary (BinaryProtocol(BinaryProtocol))
import Thrift.Transport.Empty (EmptyTransport(EmptyTransport))

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpecs)
import Test.Tasty.QuickCheck (
    Arbitrary(arbitrary, shrink), CoArbitrary,
    (===), elements, forAll, getNonZero, getPositive, oneof, resize, testProperty)

import Test.QuickCheck.Function (Function(function), functionMap)
import Test.QuickCheck.Instances ()

import Jaeger.Types
import Jaeger_Types (encode_Log, encode_Span, encode_SpanRef, encode_Tag)

import Utils (_Log, _Span, _SpanRef, _Tag)

tests :: IO TestTree
tests = do
    testSpan' <- testSpan
    return $ testGroup "jaeger-client-pinch" [
          testTag
        , testLog
        , testSpanRef
        , testSpan'
        , testProcess
        , testBatch
        , testTraceId
        , testProperty "isIso (_Wrapped @SpanId)" (isIso (_Wrapped @SpanId))
        , testEmitBatch
        , testThrift
        ]

pinchRoundTrip :: forall a. (Arbitrary a, Eq a, Pinchable a, Show a) => TestTree
pinchRoundTrip = testGroup "pinchRoundTrip" [
      testProperty "Value" $ \(a :: a) -> runParser (unpinch (pinch a)) === Right a
    , testProperty "binary" $ \(a :: a) -> decode binaryProtocol (encode binaryProtocol a) === Right a
    , testProperty "compact" $ \(a :: a) -> decode compactProtocol (encode compactProtocol a) === Right a
    ]

testTag :: TestTree
testTag = testGroup "Tag" [
      testProperty "isLens tagKey" (isLens tagKey)
    , testProperty "isPrism _StringTag" (isPrism _StringTag)
    , testProperty "isPrism _DoubleTag" (isPrism _DoubleTag)
    , testProperty "isPrism _BoolTag" (isPrism _BoolTag)
    , testProperty "isPrism _LongTag" (isPrism _LongTag)
    , testProperty "isPrism _BinaryTag" (isPrism _BinaryTag)
    , pinchRoundTrip @Tag
    ]

testLog :: TestTree
testLog = testGroup "Log" [
      testProperty "isLens logTimestamp" (isLens logTimestamp)
    , testProperty "isLens logFields" (isLens logFields)
    , pinchRoundTrip @Log
    ]

testSpanRef :: TestTree
testSpanRef = testGroup "SpanRef" [
      testProperty "isLens spanRefType" (isLens spanRefType)
    , testProperty "isLens spanRefTraceId" (isLens spanRefTraceId)
    , testProperty "isLens spanRefSpanId" (isLens spanRefSpanId)
    , pinchRoundTrip @SpanRef
    ]

testSpan :: IO TestTree
testSpan = do
    specs <- testSpecs $
        describe "spanFlags" $ do
            let s0 = span' (traceId 0 0) (0 ^. re _Wrapped) Nothing "testSpanFlags"

            it "is set to 1 when 'sampled' is set" $ do
                let s1 = s0 & spanFlags .~ Set.singleton sampled
                spanFlags' s1 `shouldBe` Right 1

            it "is set to 2 when 'debug' is set" $ do
                let s1 = s0 & spanFlags .~ Set.singleton debug
                spanFlags' s1 `shouldBe` Right 2

            it "is set to 3 when 'sampled' and 'debug' are set" $ do
                let s1 = s0 & spanFlags .~ Set.fromList [sampled, debug]
                spanFlags' s1 `shouldBe` Right 3

    return $ testGroup "Span" $ [
          testProperty "isLens spanTraceId" (isLens spanTraceId)
        , testProperty "isLens spanSpanId" (isLens spanSpanId)
        , testProperty "isLens spanParentSpanId" (isLens spanParentSpanId)
        , testProperty "isLens spanOperationName" (isLens spanOperationName)
        , testProperty "isLens spanReferences" (isLens spanReferences)
        , testProperty "isLens spanFlags" (isLens spanFlags)
        , testProperty "isLens spanStartTime" (isLens spanStartTime)
        , testProperty "isLens spanDuration" (isLens spanDuration)
        , testProperty "isLens spanTags" (isLens spanTags)
        , testProperty "isLens spanLogs" (isLens spanLogs)
        , pinchRoundTrip @Span
        ] ++ specs
  where
    spanFlags' = fmap (getField . getSpanFlags) . decode binaryProtocol . encode binaryProtocol

-- Utility to fetch field 7 ('spanFlags') as an Int32 from an encoded structure
newtype SpanFlags = SpanFlags { getSpanFlags :: Field 7 Int32 }
    deriving (Generic)

instance Pinchable SpanFlags


testProcess :: TestTree
testProcess = testGroup "Process" [
      testProperty "isLens processServiceName" (isLens processServiceName)
    , testProperty "isLens processTags" (isLens processTags)
    , pinchRoundTrip @Process
    ]

testBatch :: TestTree
testBatch = testGroup "Batch" [
      testProperty "isLens batchProcess" (isLens batchProcess)
    , testProperty "isLens batchSpans" (isLens batchSpans)
    , pinchRoundTrip @Batch
    ]

testTraceId :: TestTree
testTraceId = testGroup "TraceId" [
      testProperty "isLens traceIdLow" (isLens traceIdLow)
    , testProperty "isLens traceIdHigh" (isLens traceIdHigh)
    ]

testEmitBatch :: TestTree
testEmitBatch = testGroup "emitBatch" [
      testGroup "pinchRoundTrip" [
          testProperty "binary" $ forAll (resize 10 arbitrary) $ \a ->
              let msg = emitBatch a in
              decodeMessage binaryProtocol (encodeMessage binaryProtocol msg) === Right msg
        , testProperty "compact" $ forAll (resize 10 arbitrary) $ \a ->
              let msg = emitBatch a in
              decodeMessage compactProtocol (encodeMessage compactProtocol msg) === Right msg
        ]
    ]

testThrift :: TestTree
testThrift = testGroup "Thrift" [
      testGroup "RoundTrip" [
          -- Note: Need to use 'our' Tag type, because Thrift's Arbitrary
          -- instance generates values which are 'invalid', as in: with
          -- multiple type-value fields set.
          testProperty "Tag" $ forAll (resize 10 arbitrary) $ \a ->
            encode binaryProtocol a
                === view strict (encode_Tag (BinaryProtocol EmptyTransport) (review _Tag a))
        , testProperty "Log" $ forAll (resize 10 arbitrary) $ \a ->
            encode binaryProtocol a
                === view strict (encode_Log (BinaryProtocol EmptyTransport) (review _Log a))
        , testProperty "SpanRef" $ forAll (resize 10 arbitrary) $ \a ->
            encode binaryProtocol a
                === view strict (encode_SpanRef (BinaryProtocol EmptyTransport) (review _SpanRef a))
        , testProperty "Span" $ forAll (resize 10 arbitrary) $ \a ->
            encode binaryProtocol a
                === view strict (encode_Span (BinaryProtocol EmptyTransport) (review _Span a))
        ]
    ]

main :: IO ()
main = defaultMain =<< tests


-- Note: in the instances below, 'resize' is often used to limit the length of
-- generated lists and other values, otherwise test execution time blows up.
-- Generating 'large' values isn't useful anyway given the kind of library and
-- tests.
instance Arbitrary Batch where
    arbitrary = batch <$> arbitrary <*> resize 5 arbitrary


instance Function ByteString where
    function = functionMap BS.unpack BS.pack


instance Arbitrary (Enumeration n) where
    arbitrary = pure enum

instance CoArbitrary (Enumeration n)
instance Function (Enumeration n)


instance Arbitrary a => Arbitrary (Field n a) where
    arbitrary = putField <$> arbitrary
    shrink = map putField . shrink . getField

instance CoArbitrary a => CoArbitrary (Field n a)
instance Function a => Function (Field n a)


instance Arbitrary Log where
    arbitrary = log <$> arbitrary <*> resize 2 arbitrary

instance CoArbitrary Log
instance Function Log


instance Arbitrary Process where
    arbitrary = process <$> resize 10 arbitrary <*> resize 2 arbitrary

instance CoArbitrary Process
instance Function Process


instance Arbitrary Span where
    arbitrary = span <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> resize 10 arbitrary
                     <*> resize 5 arbitrary
                     <*> resize 10 arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> resize 5 arbitrary
                     <*> resize 5 arbitrary

instance CoArbitrary Span
instance Function Span


instance Arbitrary SpanId where
    arbitrary = view _Unwrapped . getNonZero <$> arbitrary
    shrink = map (view _Unwrapped) . shrink . view _Wrapped

instance CoArbitrary SpanId
instance Function SpanId


instance Arbitrary Flag where
    arbitrary = elements [ sampled, debug ]

instance CoArbitrary Flag
instance Function Flag


instance Arbitrary SpanRef where
    arbitrary = spanRef <$> arbitrary <*> arbitrary <*> arbitrary

instance CoArbitrary SpanRef
instance Function SpanRef


instance Arbitrary SpanRefType where
    arbitrary = elements [ childOf, followsFrom ]

instance CoArbitrary SpanRefType
instance Function SpanRefType

instance Arbitrary Tag where
    arbitrary = oneof [ stringTag <$> resize 10 arbitrary <*> resize 10 arbitrary
                      , doubleTag <$> resize 10 arbitrary <*> arbitrary
                      , boolTag <$> resize 10 arbitrary <*> arbitrary
                      , longTag <$> resize 10 arbitrary <*> arbitrary
                      , binaryTag <$> resize 10 arbitrary <*> resize 20 arbitrary
                      ]

instance CoArbitrary Tag
instance Function Tag


instance Arbitrary TagType where
    arbitrary = elements [ string, double, bool, long, binary ]

instance CoArbitrary TagType
instance Function TagType


instance Arbitrary TimeSpec where
    -- Note: the lenses for TimeSpec values aren't proper lenses, because they
    -- violate the 'read what you wrote' law, because they drop granularity from
    -- ns to us
    -- To allow this anyway, the input needs to be us-rounded timestamps
    arbitrary = fromInteger . (* 1000) . (`div` 1000) . getPositive <$> arbitrary

instance CoArbitrary TimeSpec
instance Function TimeSpec


instance Arbitrary TraceId where
    arbitrary = traceId <$> arbitrary <*> arbitrary

instance CoArbitrary TraceId
instance Function TraceId
