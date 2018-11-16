module Main where

import Lib
import Options.Applicative
import Data.Semigroup ((<>))

newtype Host = Host String deriving (Eq, Show)
newtype Port = Port String deriving (Eq, Show)
data Address = Address Host Port deriving (Eq, Show)
data Args = Args Address [Address] deriving (Eq, Show)

argsParser :: Parser Args
argsParser = Args <$> ownAddressParser <*> peerAddressesParser
  where 
    ownAddressParser = fmap toAddress $ strArgument helpText
      where helpText = metavar "OWN_ADDRESS" <> help "client's address" 

    peerAddressesParser = fmap toAddressList $ strArgument helpText
      where helpText = metavar "PEER_ADDRESSES" <> help "peer addresses" 

    toAddressList :: String -> [Address] 
    toAddressList string =
      -- placeholder
      [toAddress ""]

    toAddress :: String -> Address
    toAddress string =
      -- placeholder
      Address (Host "") (Port "")

runProgramWithArgs :: Args -> IO ()
runProgramWithArgs (Args ownAddress peerAddresses) = do
  -- placeholder
  putStrLn $ show ownAddress
  putStrLn $ show peerAddresses

main :: IO ()
main = runProgramWithArgs =<< execParser opts 
  where opts = info (argsParser <**> helper) fullDesc
