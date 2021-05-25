module Quad.Docker where

import Data.Aeson
import Data.SemVer
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

newtype ContainerId = ContainerId
  { containerId :: Text
  }
  deriving (Eq, Show)

instance FromJSON ContainerId where
  parseJSON = withObject "create-container" $ \o ->
    ContainerId <$> o .: "Id"

dockerVersion :: Version
dockerVersion = version 1 4 0 [] []

buildPath :: Text -> ByteString
buildPath path = encodeUtf8 $toText dockerVersion <> path

-- | Create a docker container by sending a request to docker socket
createContainer :: CreateContainerOptions -> QuadM ContainerId
createContainer CreateContainerOptions {..} = do
  manager <- asks appHttpManager
  let img = imageText image
  let body =
        object
          [ ("Image", toJSON img),
            ("Tty", toJSON True),
            ("Labels", object [("quad", "")]),
            ("Cmd", "echo hello"),
            ("Entrypoint", toJSON [String "/bin/sh", "-c"])
          ]
  let req =
        HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath (buildPath "/containers/create")
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body

  res <- HTTP.httpBS req
  parseHTTPResponse res

startContainer :: ContainerId -> QuadM ()
startContainer (ContainerId cId) = do
  manager <- asks appHttpManager
  let path = "/containers/" <> cId <> "/start"
  let req =
        HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath (buildPath path)
          & HTTP.setRequestMethod "POST"

  void $ HTTP.httpBS req

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

-- | Parse HTTP response into a FromJSON instance
parseHTTPResponse :: (MonadIO m, FromJSON a) => HTTP.Response ByteString -> m a
parseHTTPResponse res =
  let result = eitherDecodeStrict (HTTP.getResponseBody res)
   in case result of
        Left e -> throwString e
        Right result' -> pure result'