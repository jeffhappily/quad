module Quad.Docker where

import Data.Aeson
import Import
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.Internal as Client.Internal
import qualified Network.HTTP.Simple as HTTP
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS

-- | Container image
newtype Image = Image
  { imageText :: Text
  }
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode
  { exitCode :: Int
  }
  deriving (Eq, Show)

newtype CreateContainerOptions = CreateContainerOptions
  { image :: Image
  }

createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
  -- the default docker socket is at /var/run/docker.sock
  manager <- newUnixManager "/var/run/docker.sock"
  let body = Null
  let req =
        HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath "/v1.40/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body

  res <- HTTP.httpBS req

  traceShowIO res

-- | Create a new @Manager@ for connections to a unix domain socket
--
-- Referencing https://github.com/denibertovic/docker-hs/blob/35e43156e4f3a0df483a262407967e17ab15217b/src/Docker/Client/Http.hs#L136-L159
newUnixManager :: FilePath -> IO Client.Manager
newUnixManager fp = do
  Client.newManager $
    Client.defaultManagerSettings
      { Client.managerRawConnection = pure openUnixSocket
      }
  where
    -- Docker seems to ignore the hostname in requests sent over unix domain
    -- sockets (and the port obviously doesn't matter either)
    openUnixSocket _ _ _ = do
      s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect s (S.SockAddrUnix fp)
      Client.Internal.makeConnection
        (SBS.recv s 8096)
        (SBS.sendAll s)
        (S.close s)