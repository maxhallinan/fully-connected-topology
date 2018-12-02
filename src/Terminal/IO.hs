module Terminal.IO
  ( readInput
  , initMessageChannel
  , handleReceivedMessage
  , printMessage
  ) where

import Control.Monad (forever)
import Control.Concurrent as Concurrent

type MessageChannel = Concurrent.MVar String

initMessageChannel :: IO MessageChannel
initMessageChannel = Concurrent.newEmptyMVar

handleReceivedMessage :: MessageChannel -> String -> IO ()
handleReceivedMessage = Concurrent.putMVar

printMessage :: MessageChannel -> IO ()
printMessage messageChannel = forever $ do
  message <- Concurrent.takeMVar messageChannel
  putStrLn $ "New message: " ++ message

readInput :: MessageChannel -> IO ()
readInput messageChannel = forever $ do
  message <- getLine
  Concurrent.putMVar messageChannel message
