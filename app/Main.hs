{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>), some)
import Data.Void (Void)
import Data.Semigroup ((<>))
import Lib
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega
import Options.Applicative ((<**>)) 
import qualified Options.Applicative as Opt
import qualified System.Network as Net

newtype Host = Host String deriving (Eq, Show)
newtype Port = Port String deriving (Eq, Show)
data Address = Address Host Port deriving (Eq, Show)
data Args = Args Address [Address] deriving (Eq, Show)

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

runProgramWithArgs :: Args -> IO ()
runProgramWithArgs (Args ownAddress peerAddresses) = do
  putStrLn $ show ownAddress
  putStrLn $ show peerAddresses

main :: IO ()
main = runProgramWithArgs =<< Opt.execParser opts 
  where opts = Opt.info (argsParser <**> Opt.helper) Opt.fullDesc
