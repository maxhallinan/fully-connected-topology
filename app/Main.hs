{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>), some)
import Control.Concurrent (forkFinally, forkIO, threadDelay)
import qualified Control.Exception as Exc
import Control.Monad ((<=<), forever, void)
import qualified Data.ByteString.Char8 as ByteChar
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Void (Void)
import Data.Semigroup ((<>))
import Lib
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetByte
import qualified Control.Concurrent.Async as Async

newtype Host = Host String deriving (Eq, Show)
newtype Port = Port String deriving (Eq, Show)
data Address = Address Host Port deriving (Eq)
data Args = Args Address [Address] deriving (Eq, Show)

instance Show Address where
  show (Address host port) = (coerce host) ++ ":" ++ (coerce port)

type Parsec = Mega.Parsec Void String

addressParser :: Parsec Address
addressParser = do
  host <- some (Mega.alphaNumChar <|> Mega.char '.')
  Mega.char ':'
  port <- some Mega.digitChar
  return $ Address (Host host) (Port port)

peersParser :: Parsec [Address]
peersParser = addressParser `Mega.sepBy` Mega.char ','

argsParser :: Opt.Parser Args
argsParser = Args <$> ownAddressParser <*> peerAddressesParser
  where
    ownAddressParser = Opt.argument argReader helpText
      where
        argReader = Opt.maybeReader (Mega.parseMaybe addressParser)
        helpText  = Opt.metavar "OWN_ADDRESS" <> Opt.help "client's address"

    peerAddressesParser = Opt.argument argReader helpText
      where
        argReader = Opt.maybeReader (Mega.parseMaybe peersParser)
        helpText  = Opt.metavar "PEER_ADDRESSES" <> Opt.help "peer addresses"

optParserInfo :: Opt.ParserInfo Args
optParserInfo = Opt.info (argsParser <**> Opt.helper) Opt.fullDesc

-- TODO
-- [X]. open socket on own host and port
-- []. receive messages from peers to self
-- []. connect to each of the peers
-- []. send messages from self to peers
runProgramWithArgs :: Args -> IO ()
runProgramWithArgs (Args ownAddress peerAddresses) = Net.withSocketsDo $ do
  let socket = startServer ownAddress
  -- Close the sockets if an exception is raised during the main loop.
  Exc.bracket (startServer ownAddress) stopServer (mainLoop peerAddresses)
  where
    startServer :: Address -> IO Net.Socket
    startServer = openSocket <=< resolveAddress

    stopServer :: Net.Socket -> IO ()
    stopServer = Net.close

    mainLoop :: [Address] -> Net.Socket -> IO ()
    mainLoop peerAddresses ownSocket = do
      -- fork managed threads
      async1 <- Async.async $ listenToPeers ownSocket
      async2 <- Async.async $ talkToPeers peerAddresses
      -- wait for these threads to complete before continuing
      void $ Async.wait async1
      void $ Async.wait async2
 
    listenToPeers :: Net.Socket -> IO ()
    listenToPeers ownSocket = forever $ do
      (peerSocket, peerAddress) <- Net.accept ownSocket
      forkFinally (listenToPeer (peerSocket, peerAddress)) (\_ -> Net.close peerSocket)
      putStrLn $ "Now receiving messages from " ++ show peerAddress

    listenToPeer :: (Net.Socket, Net.SockAddr) -> IO ()
    listenToPeer (peerSocket, peerAddress) = do
      message <- NetByte.recv peerSocket 1024
      putStrLn $ "New message from " ++ (show peerAddress) ++ ": " ++ (ByteChar.unpack message)

    talkToPeers :: [Address] -> IO ()
    talkToPeers peerAddress = do
      connectToPeers peerAddress

    connectToPeers :: [Address] -> IO ()
    connectToPeers peerAddresses = do
      putStrLn "connecting to peers"
      connections <- connect peerAddresses
      return $ fold connections
      where connect = traverse (connectToPeer <=< resolveAddress)

    resolveAddress :: Address -> IO Net.AddrInfo
    resolveAddress (Address host port) = do
      let hints = Net.defaultHints { Net.addrFlags = [Net.AI_PASSIVE]
                                   , Net.addrSocketType = Net.Stream
                                   }
      (addressInfo : _) <- Net.getAddrInfo
                            (Just hints)
                            (Just $ coerce host)
                            (Just $ coerce port)
      return addressInfo

    openSocket :: Net.AddrInfo -> IO Net.Socket
    openSocket addressInfo = do
      -- create a socket
      socket <- Net.socket
                  (Net.addrFamily addressInfo)
                  (Net.addrSocketType addressInfo)
                  (Net.addrProtocol addressInfo)
      -- bind socket to the given address
      Net.bind socket (Net.addrAddress addressInfo)
      -- start listening for connection request
      -- set the max queue size for connection requests to 5
      Net.listen socket 5
      putStrLn $ "Now listening on " ++ (show ownAddress)
      return socket

    connectToPeer :: Net.AddrInfo -> IO ()
    connectToPeer addressInfo = do
      putStrLn $ "connecting to peer: " ++ (show $ Net.addrAddress addressInfo)
      peerSocket <- Net.socket
                  (Net.addrFamily addressInfo)
                  (Net.addrSocketType addressInfo)
                  (Net.addrProtocol addressInfo)
      void $ forkFinally (connect peerSocket) (r peerSocket)
      where 
          r peerSocket e = do
            putStrLn $ show e
            retryConnect peerSocket

          --- what's happening at the moment is that I connect to the socket,
          --  send it the hello world message, and then the function finishes.
          --  so the thread finishes and the `r` function is called by forkFinally.
          --  This restarts the peer connection process.
          --  What I want is for the thread to sleep while it waits for a message
          --  to send.
          --  Could do this with an MVar.
          connect socket = do 
            Net.connect socket $ Net.addrAddress addressInfo
            NetByte.send socket "Hello, World!"
            putStrLn $ "connected to " ++ (show $ Net.addrAddress addressInfo)

          retryConnect socket = do
            Net.close socket
            putStrLn $ "retrying connection to: " ++ (show $ Net.addrAddress addressInfo)
            threadDelay 1500000
            connectToPeer addressInfo

main :: IO ()
main = runProgramWithArgs =<< Opt.execParser optParserInfo
