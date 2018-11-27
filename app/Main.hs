{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import qualified Control.Concurrent.Async as Async
import qualified Network.FullyConnected as FullyConnected
import qualified Terminal.Args as Args

main :: IO ()
main = runProgramWithArgs =<< Args.parseArgs

runProgramWithArgs :: Args.Args -> IO ()
runProgramWithArgs args = do
  let ownAddress    = Args.ownAddress args
      peerAddresses = Args.peerAddresses args
  listenThread <- Async.async $ FullyConnected.listenToPeers handleMessage ownAddress 
  -- placeholder message to be replaced with input from stdin
  talkThread   <- Async.async $ FullyConnected.talkToPeers peerAddresses "Hello, World!"
  void $ Async.wait listenThread
  void $ Async.wait talkThread

handleMessage :: String -> IO ()
handleMessage message = putStrLn $ "New message: " ++ message
