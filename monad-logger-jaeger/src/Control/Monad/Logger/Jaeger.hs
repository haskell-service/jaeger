{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module:      Control.Monad.Logger.Jaeger
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, StandaloneDeriving, TypeFamilies, UndecidableInstances
--
-- A "Control.Monad.Logger" middleware which emits log entries to
-- <https://uber.github.io/jaeger/ Jaeger> 'Jaeger.Types.Span's.
--
-- Also defines orphan 'MonadLogger' instances for 'JaegerT', 'JaegerTraceT' and
-- the likes, as well as 'MonadJaeger' and 'MonadJaegerTrace' instances for
-- 'LoggingT' etc.

module Control.Monad.Logger.Jaeger (
    -- * Monad transformer
      JaegerLoggingT
    , runJaegerLoggingT
    -- * Plumbing
    , logTags
    ) where

import Control.Monad (when)
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Jaeger.Class (MonadJaeger)
import Control.Monad.JaegerTrace.Class (MonadJaegerTrace, addLog)
import Control.Monad.Logger (
    Loc(loc_end, loc_filename, loc_module, loc_package, loc_start),
    LogLevel(LevelDebug, LevelError, LevelInfo, LevelWarn, LevelOther),
    LogSource, LoggingT, MonadLogger(monadLoggerLog), NoLoggingT, ToLogStr,
    defaultLoc, toLogStr)
import Control.Monad.Reader.Class (MonadReader(ask, local, reader))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (
    MonadBaseControl(liftBaseWith, restoreM), StM, ComposeSt,
    defaultLiftBaseWith, defaultRestoreM,
    MonadTransControl(liftWith, restoreT), StT, defaultLiftWith, defaultRestoreT)
import Control.Monad.Trans.Jaeger (JaegerT, NoJaegerT)
import Control.Monad.Trans.JaegerTrace (JaegerTraceT, NoJaegerTraceT)
import Control.Monad.Trans.Reader (ReaderT, mapReaderT, runReaderT)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Writer.Class (MonadWriter)

import qualified Data.Text as Text hiding (singleton)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.Builder.Int as Text

import System.Log.FastLogger (fromLogStr)

import Jaeger.Types (Tag, stringTag)
import Jaeger.OpenTracing.Tags (logMessage, logStack)

-- | A monad transformer which captures 'MonadLogger' logging and appends it to 'MonadJaegerTrace' 'Jaeger.Types.Span's.
--
-- /Note:/ The 'MonadLogger' instance doesn't resolve the 'MonadLogger'
-- constraint: it merely passes log entries through to some other handler, after
-- appending said entries to the current 'Jaeger.Types.Span'.
newtype JaegerLoggingT m a = JaegerLoggingT { unJaegerLoggingT :: ReaderT LogLevel m a }
    deriving (
        Functor, Applicative, Monad,
        MonadCont, MonadError e, MonadState s, MonadWriter w,
        MonadTrans,
        MonadIO,
        MonadCatch, MonadMask, MonadThrow,
        MonadBase b,
        MonadJaeger, MonadJaegerTrace)

deriving instance MonadResource m => MonadResource (JaegerLoggingT m)

instance MonadReader r m => MonadReader r (JaegerLoggingT m) where
    ask = lift ask
    local = mapJaegerLoggingT . local
    reader = lift . reader

instance MonadRWS r w s m => MonadRWS r w s (JaegerLoggingT m)

mapJaegerLoggingT :: (m a -> n b) -> JaegerLoggingT m a -> JaegerLoggingT n b
mapJaegerLoggingT f = JaegerLoggingT . mapReaderT f . unJaegerLoggingT

instance MonadTransControl JaegerLoggingT where
    type StT JaegerLoggingT a = StT (ReaderT LogLevel) a
    liftWith = defaultLiftWith JaegerLoggingT unJaegerLoggingT
    restoreT = defaultRestoreT JaegerLoggingT

instance MonadBaseControl b m => MonadBaseControl b (JaegerLoggingT m) where
    type StM (JaegerLoggingT m) a = ComposeSt JaegerLoggingT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance (MonadBase IO m, MonadLogger m, MonadJaegerTrace m) => MonadLogger (JaegerLoggingT m) where
    monadLoggerLog loc logSource logLevel msg = do
        minLevel <- JaegerLoggingT ask
        when (logLevel >= minLevel) $
            addLog $ logTags loc logSource logLevel msg
       -- Log to the proper log handler
        lift $ monadLoggerLog loc logSource logLevel msg
    {-# INLINE monadLoggerLog #-}

-- | Run a 'JaegerLoggingT' action, capturing all log entries at or above the given threshold.
runJaegerLoggingT :: JaegerLoggingT m a  -- ^ Action to run
                  -> LogLevel  -- ^ Log level threshold. When e.g. 'LevelInfo', 'LevelDebug' messages will not be captured
                  -> m a
runJaegerLoggingT = runReaderT . unJaegerLoggingT
{-# INLINE runJaegerLoggingT #-}

-- | Convert a log entry to a list of 'Tag's.
--
-- This is useful when implementing a custom collapsed monad to which one wants
-- to add Jaeger log capturing.
logTags :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> [Tag]
logTags loc logSource logLevel msg = catMaybes [
    Just $ stringTag "level" $ case logLevel of
        LevelDebug -> "debug"
        LevelInfo -> "info"
        LevelWarn -> "warn"
        LevelError -> "error"
        LevelOther lvl -> lvl
  , if Text.null logSource then Nothing else Just (stringTag "source" logSource)
  , Just $ logMessage $ Text.decodeUtf8 $ fromLogStr $ toLogStr msg
  , if loc == defaultLoc
      then Nothing
      else Just $ logStack $ Text.toStrict $ Text.toLazyTextWith 128 $
             Text.fromString (loc_package loc)   -- 20
          <> Text.singleton ':'                  --  1
          <> Text.fromString (loc_module loc)    -- 30
          <> Text.singleton '@'                  --  1
          <> Text.fromString (loc_filename loc)  -- 35
          <> Text.singleton ':'                  --  1
          <> Text.decimal (fst $ loc_start loc)  --  3
          <> Text.singleton ','                  --  1
          <> Text.decimal (snd $ loc_start loc)  --  3
          <> Text.singleton '-'                  --  1
          <> Text.decimal (fst $ loc_end loc)    --  3
          <> Text.singleton ','                  --  1
          <> Text.decimal (snd $ loc_end loc)    --  3
                                                 -- --
                                                 --103 <- estimated required buffer size, rounded up to 128
  ]

instance MonadJaeger m => MonadJaeger (LoggingT m)
instance MonadJaegerTrace m => MonadJaegerTrace (LoggingT m)
instance MonadJaeger m => MonadJaeger (NoLoggingT m)
instance MonadJaegerTrace m => MonadJaegerTrace (NoLoggingT m)

instance MonadLogger m => MonadLogger (JaegerT m)
instance MonadLogger m => MonadLogger (NoJaegerT m)
instance MonadLogger m => MonadLogger (JaegerTraceT m)
instance MonadLogger m => MonadLogger (NoJaegerTraceT m)
