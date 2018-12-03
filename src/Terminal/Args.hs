{-# LANGUAGE OverloadedStrings #-}

module Terminal.Args 
  ( Args
  , ownAddress
  , parseArgs
  , peerAddresses
  ) where

import Control.Applicative ((<|>), some)
import Data.Void (Void)
import Network.FullyConnected as FullyConnected
import Options.Applicative ((<**>))
import qualified Options.Applicative as Opt
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega

data Args = Args { ownAddress :: FullyConnected.Address
                 , peerAddresses :: [FullyConnected.Address] 
                 } deriving (Eq, Show)

parseArgs :: IO Args
parseArgs = Opt.execParser argsParserWithInfo

type Parsec = Mega.Parsec Void String

argsParserWithInfo :: Opt.ParserInfo Args
argsParserWithInfo = Opt.info (argsParser <**> Opt.helper) Opt.fullDesc

argsParser :: Opt.Parser Args
argsParser = Args <$> ownAddressParser <*> peerAddressesParser

ownAddressParser = Opt.argument argReader helpText
  where
    argReader = Opt.maybeReader (Mega.parseMaybe FullyConnected.addressParser)
    helpText  = Opt.metavar "OWN_ADDRESS" <> Opt.help "the local address of the client"

peerAddressesParser = Opt.argument argReader helpText
  where
    argReader = Opt.maybeReader (Mega.parseMaybe peersParser)
    helpText  = Opt.metavar "PEER_ADDRESSES" <> Opt.help "a comma-separated list of peer addresses"

peersParser :: Parsec [FullyConnected.Address]
peersParser = FullyConnected.addressParser `Mega.sepBy` Mega.char ','
