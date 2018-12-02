{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forever, void)
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Network.FullyConnected as FullyConnected
import qualified Terminal.Args as Args
import qualified Terminal.IO as Terminal.IO

main :: IO ()
main = runProgramWithArgs =<< Args.parseArgs

runProgramWithArgs :: Args.Args -> IO ()
runProgramWithArgs args = do
  let ownAddress    = Args.ownAddress args
      peerAddresses = Args.peerAddresses args

  connections <- FullyConnected.connectToPeers peerAddresses
  receiveChannel <- Terminal.IO.initMessageChannel
  sendChannel <- Terminal.IO.initMessageChannel

  listenThread <- Async.async $ FullyConnected.listenToPeers 
                    (Terminal.IO.handleReceivedMessage receiveChannel) 
                    ownAddress
  -- talkThread <- Async.async $ FullyConnected.talkToPeers connections "Hello, World!"
  talkThread <- Async.async $ broadcastMessage connections sendChannel

  printMessageThread <- Async.async $ Terminal.IO.printMessage receiveChannel
  userInputThread <- Async.async $ Terminal.IO.readInput sendChannel

  void $ Async.wait printMessageThread
  void $ Async.wait listenThread
  void $ Async.wait userInputThread
  void $ Async.wait talkThread

handleMessage :: Concurrent.MVar String -> String -> IO ()
handleMessage = Concurrent.putMVar

broadcastMessage :: FullyConnected.Connections -> Concurrent.MVar String -> IO ()
broadcastMessage connections messageChannel = forever $ do
  message <- Concurrent.takeMVar messageChannel
  foo message
  where foo message = do
          c <- Concurrent.takeMVar connections
          putStrLn $ "Message sent: " ++ message
          putStrLn $ "Current number of connections: " ++ (show $ length c)

logConnection :: FullyConnected.Connections -> IO ()
logConnection cs = forever $ do
  c <- Concurrent.takeMVar cs
  putStrLn $ "connection added, total connections: " ++ (show $ length c)
