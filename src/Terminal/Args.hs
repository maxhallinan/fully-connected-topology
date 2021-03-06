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
    argReader = Opt.maybeReader FullyConnected.parseAddress
    helpText  = Opt.metavar "<own-address>" <> Opt.help "The local address of the client"

peerAddressesParser = some $ Opt.argument argReader helpText
  where
    argReader = Opt.maybeReader FullyConnected.parseAddress
    helpText  = Opt.metavar "<peer-address>..." <> Opt.help "One or more peer addresses"
