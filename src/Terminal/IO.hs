module Terminal.IO
  ( inputMessage
  , outputMessage
  ) where

import Control.Monad (forever)

outputMessage :: String -> IO ()
outputMessage message = putStrLn $ "New message: " ++ message

inputMessage :: (String -> IO ()) -> IO ()
inputMessage handleInput = forever $ do
  message <- getLine
  handleInput message
