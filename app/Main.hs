{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>), some)
import Control.Concurrent (forkIO)
import qualified Control.Exception as Exc
import Control.Monad ((<=<), void)
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
  let sockets = openSockets (ownAddress, peerAddresses)
  -- Close the sockets if an exception is raised during the main loop.
  Exc.bracket sockets closeSockets mainLoop
  where
    openSockets :: (Address, [Address]) -> IO (Net.Socket, [Net.Socket])
    openSockets (ownAddress, peerAddresses) = do
      ownSocket <- startListening ownAddress
      peerSockets <- connectToPeers peerAddresses
      return (ownSocket, peerSockets)

    closeSockets :: (Net.Socket, [Net.Socket]) -> IO ()
    closeSockets (ownSocket, peerSockets) = do
      Net.close ownSocket 
      fold $ fmap Net.close peerSockets      

    mainLoop :: (Net.Socket, [Net.Socket]) -> IO ()
    mainLoop (ownSocket, peerSockets) = do
      listenToPeers ownSocket
      talkToPeers peerSockets
      mainLoop (ownSocket, peerSockets)

    startListening :: Address -> IO Net.Socket
    startListening = openSocket <=< resolveAddress

    connectToPeers :: [Address] -> IO [Net.Socket]
    connectToPeers = traverse (connectToPeer <=< resolveAddress)

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

    connectToPeer :: Net.AddrInfo -> IO Net.Socket
    connectToPeer addressInfo = do
      socket <- Net.socket
                  (Net.addrFamily addressInfo)
                  (Net.addrSocketType addressInfo)
                  (Net.addrProtocol addressInfo)
      Net.connect socket $ Net.addrAddress addressInfo
      return socket

    listenToPeers :: Net.Socket -> IO ()
    listenToPeers ownSocket = do
      (peerSocket, peerAddress) <- Net.accept ownSocket
      forkIO $ listenToPeer (peerSocket, peerAddress)
      putStrLn $ "Now receiving messages from " ++ show peerAddress

    listenToPeer :: (Net.Socket, Net.SockAddr) -> IO ()
    listenToPeer (peerSocket, peerAddress) = do
      message <- NetByte.recv peerSocket 1024
      putStrLn $ "New message from " ++ (show peerAddress) ++ ": " ++ (ByteChar.unpack message)
      listenToPeer (peerSocket, peerAddress)

    talkToPeers :: [Net.Socket] -> IO ()
    talkToPeers peerSockets = do
      return ()

main :: IO ()
main = runProgramWithArgs =<< Opt.execParser optParserInfo
