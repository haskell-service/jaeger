{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#include "cabal_macros.h"

-- |
-- Module:      Jaeger.Process
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: CPP, OverloadedStrings
--
-- Utilities to create 'Process' values, based on the running process'
-- environment.

module Jaeger.Process (
      Jaeger.Process.process
    , processTags
    ) where

import System.Environment (getProgName)

import System.Posix.Process (getProcessID, getParentProcessID)
import System.Posix.Unistd (SystemID(systemName, nodeName, release, version, machine), getSystemID)

import Data.Text (Text)
import qualified Data.Text as Text

import Jaeger.Types (Process, Tag, longTag, process, stringTag)

-- | Construct a 'Process', setting the service name to the process name and tags to 'processTags'.
process :: IO Process
process = Jaeger.Types.process <$> fmap Text.pack getProgName
                               <*> processTags
-- | Generate some generic 'Process' 'Tag's.
processTags :: IO [Tag]
processTags = sequence [ longTag "process.pid" <$> fmap fromIntegral getProcessID
                       , longTag "process.parentPid" <$> fmap fromIntegral getParentProcessID
                       , stringTag "process.uname" <$> getUname
                       , pure $ stringTag "jaeger.version" VERSION_jaeger_types
                       , stringTag "hostname" . Text.pack . nodeName <$> getSystemID
                       -- TODO , stringTag "ip" _
                       ]

getUname :: IO Text
getUname = do
    s <- getSystemID
    return $ Text.intercalate " " $ map Text.pack [ systemName s
                                                  , nodeName s
                                                  , release s
                                                  , version s
                                                  , machine s
                                                  ]
