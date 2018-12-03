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
  let ownAddress      = Args.ownAddress args
      peerAddresses   = Args.peerAddresses args
  connections         <- FullyConnected.connect peerAddresses
  receiveChannel      <- Terminal.IO.initMessageChannel
  sendChannel         <- Terminal.IO.initMessageChannel
  listenThread        <- Async.async $ FullyConnected.listen
                          (Terminal.IO.handleReceivedMessage receiveChannel)
                          ownAddress
  broadcastThread     <- Async.async $ broadcastMessage connections sendChannel
  printMessageThread  <- Async.async $ Terminal.IO.printMessage receiveChannel
  userInputThread     <- Async.async $ Terminal.IO.readInput sendChannel
  void $ Async.wait listenThread
  void $ Async.wait broadcastThread
  void $ Async.wait printMessageThread
  void $ Async.wait userInputThread

broadcastMessage :: FullyConnected.Connections -> Concurrent.MVar String -> IO ()
broadcastMessage connections messageChannel = forever $ do
  message <- Concurrent.takeMVar messageChannel
  broadcast message
  where broadcast message = FullyConnected.broadcast connections message
