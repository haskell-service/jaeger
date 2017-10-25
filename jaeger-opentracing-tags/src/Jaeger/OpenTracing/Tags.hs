{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Jaeger.OpenTracing.Tags
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: OverloadedStrings
--
-- Utilities to construct all of the standard OpenTracing tags (see
-- <https://github.com/opentracing/specification/blob/ea2b29f46500827083e42b7a0d65bd422ccc28c1/semantic_conventions.md>).
--
-- /Note:/ Documentation is copied straight from the conventions document.

module Jaeger.OpenTracing.Tags (
    -- * Standard tags
      component
    , dbInstance, dbStatement, dbType, dbUser
    , Jaeger.OpenTracing.Tags.error
    , httpMethod, httpStatusCode, httpURL
    , messageBusDestination
    , peerAddress, peerHostname, peerIPv4, peerIPv6, peerPort, peerService
    , samplingPriority
    , SpanKind(..), spanKind
    -- ** Utilities for tag sets
    , peerTags
    , rpcClient, rpcServer
    , messageBusConsumer, messageBusProducer
    -- * Log tags
    , logErrorKind
    , logEvent, logError
    , logMessage
    , logStack
    ) where

import Data.Int (Int64)
import Data.Maybe (catMaybes)

import Data.Text (Text)

import Control.Lens ((^.))

import Jaeger.Types (Tag, boolTag, longTag, stringTag)

import Jaeger.OpenTracing.Peer (Peer)
import qualified Jaeger.OpenTracing.Peer as Peer

-- | The software package, framework, library, or module that generated the
-- associated Span.
--
-- E.g., "grpc", "django", "JDBI".
component :: Text -> Tag
component = stringTag "component"

-- | Database instance name.
--
-- E.g., In java, if the jdbc.url="jdbc:mysql://127.0.0.1:3306/customers",
-- the instance name is "customers".
dbInstance :: Text -> Tag
dbInstance = stringTag "db.instance"

-- | A database statement for the given database type.
--
-- E.g., for db.type="sql", "SELECT * FROM wuser_table"; for db.type="redis",
-- "SET mykey 'WuValue'".
dbStatement :: Text -> Tag
dbStatement = stringTag "db.statement"

-- | Database type.
--
-- For any SQL database, "sql". For others, the lower-case database category,
-- e.g. "cassandra", "hbase", or "redis".
dbType :: Text -> Tag
dbType = stringTag "db.type"

-- | Username for accessing database.
--
-- E.g., "readonly_user" or "reporting_user".
dbUser :: Text -> Tag
dbUser = stringTag "db.user"

-- | True if and only if the application considers the operation represented by
-- the Span to have failed.
error :: Bool -> Tag
error = boolTag "error"

-- | HTTP method of the request for the associated Span.
--
-- E.g., "GET", "POST".
httpMethod :: Text -> Tag
httpMethod = stringTag "http.method"

-- | HTTP response status code for the associated Span.
--
-- E.g., 200, 503, 404.
httpStatusCode :: Int64 -> Tag
httpStatusCode = longTag "http.status_code"

-- | URL of the request being handled in this segment of the trace, in standard
-- URI format.
--
-- E.g., "https://domain.net/path/to?resource=here"
httpURL :: Text -> Tag
httpURL = stringTag "http.url"

-- | An address at which messages can be exchanged.
--
-- E.g. A Kafka record has an associated "topic name" that can be extracted by
-- the instrumented producer or consumer and stored using this tag.
messageBusDestination :: Text -> Tag
messageBusDestination = stringTag "message_bus.destination"

-- | Remote "address", suitable for use in a networking client library.
--
-- This may be a "ip:port", a bare "hostname", a FQDN, or even a JDBC substring
-- like "mysql://prod-db:3306".
peerAddress :: Text -> Tag
peerAddress = stringTag "peer.address"

-- | Remote hostname.
--
-- E.g., "opentracing.io", "internal.dns.name".
peerHostname :: Text -> Tag
peerHostname = stringTag "peer.hostname"

-- | Remote IPv4 address as a .-separated tuple.
--
-- E.g., "127.0.0.1".
peerIPv4 :: Text -> Tag
peerIPv4 = stringTag "peer.ipv4"

-- | Remote IPv6 address as a string of colon-separated 4-char hex tuples.
--
-- E.g., "2001:0db8:85a3:0000:0000:8a2e:0370:7334".
peerIPv6 :: Text -> Tag
peerIPv6 = stringTag "peer.ipv6"

-- | Remote port.
--
-- E.g., 80.
peerPort :: Int64 -> Tag
peerPort = longTag "peer.port"

-- | Remote service name (for some unspecified definition of "service").
--
-- E.g., "elasticsearch", "a_custom_microservice", "memcache".
peerService :: Text -> Tag
peerService = stringTag "peer.service"

-- | If greater than 0, a hint to the Tracer to do its best to capture the
-- trace. If 0, a hint to the trace to not-capture the trace.
--
-- If absent, the Tracer should use its default sampling mechanism.
samplingPriority :: Int64 -> Tag
samplingPriority = longTag "sampling.priority"


-- | Enumeration of standard "span.kind" tag values.
data SpanKind = Client | Server | Producer | Consumer
    deriving (Show, Eq, Enum, Bounded)

-- | Either "client" or "server" for the appropriate roles in an RPC, and
-- "producer" or "consumer" for the appropriate roles in a messaging scenario.
spanKind :: SpanKind -> Tag
spanKind s = stringTag "span.kind" $ case s of
    Client -> "client"
    Server -> "server"
    Producer -> "producer"
    Consumer -> "consumer"


-- | Tag set related to a 'Peer'.
peerTags :: Peer -> [Tag]
peerTags peer = catMaybes [ peerAddress <$> peer ^. Peer.peerAddress
                          , peerHostname <$> peer ^. Peer.peerHostname
                          , peerIPv4 <$> peer ^. Peer.peerIPv4
                          , peerIPv6 <$> peer ^. Peer.peerIPv6
                          , peerPort <$> peer ^. Peer.peerPort
                          , peerService <$> peer ^. Peer.peerService
                          ]

-- | Model an RPC client.
rpcClient :: Peer -> [Tag]
rpcClient peer = spanKind Client
               : peerTags peer

-- | Model an RPC server.
rpcServer :: Peer -> [Tag]
rpcServer peer = spanKind Server
               : peerTags peer

-- | Model a producer of message bus messages.
messageBusProducer :: Text  -- ^ Destination (see 'messageBusDestination')
                   -> Peer  -- ^ Message bus service
                   -> [Tag]
messageBusProducer destination peer = spanKind Producer
                                    : messageBusDestination destination
                                    : peerTags peer

-- | Model a consumer of message bus messages.
messageBusConsumer :: Text  -- ^ Source (see 'messageBusDestination')
                   -> Peer  -- ^ Message bus service
                   -> [Tag]
messageBusConsumer destination peer = spanKind Consumer
                                    : messageBusDestination destination
                                    : peerTags peer


-- | The type or "kind" of an error (only for event="error" logs).
--
-- E.g., "Exception", "OSError".
logErrorKind :: Text -> Tag
logErrorKind = stringTag "error.kind"

-- | A stable identifier for some notable moment in the lifetime of a Span.
--
-- For instance, a mutex lock acquisition or release or the sorts of lifetime
-- events in a browser page load described in the Performance.timing
-- specification.
--
-- E.g., from Zipkin, "cs", "sr", "ss", or "cr". Or, more generally,
-- "initialized" or "timed out".
--
-- For errors, "error".
logEvent :: Text -> Tag
logEvent = stringTag "event"

-- | The "error" event.
logError :: Tag
logError = logEvent "error"

-- | A concise, human-readable, one-line message explaining the event.
--
-- E.g., "Could not connect to backend", "Cache invalidation succeeded".
logMessage :: Text -> Tag
logMessage = stringTag "message"

-- | A stack trace in platform-conventional format; may or may not pertain to
-- an error.
--
-- E.g., "File \"example.py\", line 7, in \<module\>\ncaller()\nFile
-- \"example.py\", line 5, in caller\ncallee()\nFile \"example.py\", line 2, in
-- callee\nraise Exception(\"Yikes\")\n"
logStack :: Text -> Tag
logStack = stringTag "stack"
