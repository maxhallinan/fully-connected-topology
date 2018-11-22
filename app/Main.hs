{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>), some)
import Control.Concurrent (forkIO)
import qualified Control.Exception as Exc
import Control.Monad ((<=<), void)
import qualified Data.ByteString.Char8 as ByteChar
import Data.Coerce (coerce)
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
  ownAddressInfo <- resolve ownAddress
  -- close the socket if an exception is raised during computation of `loop`
  Exc.bracket (open ownAddressInfo) Net.close (mainLoop ownAddress)
  where
    resolve :: Address -> IO Net.AddrInfo
    resolve (Address host port) = do
      let hints = Net.defaultHints { Net.addrFlags = [Net.AI_PASSIVE]
                                   , Net.addrSocketType = Net.Stream
                                   }
      (addressInfo : _) <- Net.getAddrInfo
                            (Just hints)
                            (Just $ coerce host)
                            (Just $ coerce port)
      return addressInfo

    open :: Net.AddrInfo -> IO Net.Socket
    open addressInfo = do
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
      return socket

    connectToPeer :: Net.AddrInfo -> IO Net.Socket
    connectToPeer addressInfo = do
      socket <- Net.socket
                  (Net.addrFamily addressInfo)
                  (Net.addrSocketType addressInfo)
                  (Net.addrProtocol addressInfo)
      forkIO (Net.connect socket $ Net.addrAddress addressInfo)
      return socket

    connectPeers :: [Address] -> IO [Net.Socket]
    connectPeers = traverse (connectToPeer <=< resolve)

    listenToPeer :: (Net.Socket, Net.SockAddr) -> IO ()
    listenToPeer (peerSocket, peerAddress) = do
      message <- NetByte.recv peerSocket 1024
      putStrLn $ "New message from " ++ (show peerAddress) ++ ": " ++ (ByteChar.unpack message)
      listenToPeer (peerSocket, peerAddress)

    acceptConnections :: Net.Socket -> IO ()
    acceptConnections ownSocket = do
      (peerSocket, peerAddress) <- Net.accept ownSocket
      forkIO $ listenToPeer (peerSocket, peerAddress)
      putStrLn $ "Now receiving messages from " ++ show peerAddress

    mainLoop :: Address -> Net.Socket -> IO ()
    mainLoop ownAddress ownSocket = do
      putStrLn $ "Now listening on " ++ (show ownAddress)
      let go socket = do
            acceptConnections socket
            go socket
      go ownSocket

main :: IO ()
main = runProgramWithArgs =<< Opt.execParser optParserInfo
