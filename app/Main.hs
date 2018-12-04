{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import qualified Control.Concurrent.Async as Async
import qualified Network.FullyConnected as FullyConnected
import qualified Terminal.Args as Args
import qualified Terminal.IO as Terminal.IO

main :: IO ()
main = Args.parseArgs >>= runProgramWithArgs

runProgramWithArgs :: Args.Args -> IO ()
runProgramWithArgs args = do
  let ownAddress = Args.ownAddress args
      peerAddresses = Args.peerAddresses args
  connections <- FullyConnected.connect peerAddresses
  outputThread <- Async.async $ FullyConnected.listen Terminal.IO.outputMessage ownAddress
  let broadcast = FullyConnected.broadcast connections
  inputThread <- Async.async $ Terminal.IO.inputMessage broadcast
  void $ Async.wait inputThread
  void $ Async.wait outputThread
