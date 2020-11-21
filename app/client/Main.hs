-- | Клиент для игры

module Main where

import Api
import Lib
import Types

import Control.Monad
import Control.Monad.IO.Class
import Text.Read

gameClient :: ClientM ()
gameClient = do
  liftIO $ putStrLn "Hello world!"
  f <- getField
  liftIO $ do
    putStrLn $ "Turn: " ++ show (turn f)
{-  todo как выводить?
    forM_ (oldCombinations f) $ \(c,a) -> do
      putStrLn $ show c ++ " -> " ++ show a
    print $ askedCombination f
-}
  ans <- liftIO getLine
  case ans of
    "quit" -> pure ()
    "1" -> liftIO $ putStrLn "1"
    Just x y ->
{-
    "no" -> postMove NoGuess >> gameClient
    _ -> case readMaybe ans of
      Just x -> postMove (Guess x) >> gameClient
      Nothing -> gameClient
-}

main :: IO ()
main = do
  let baseUrl = BaseUrl Http "localhost" 8081 ""
  res <- runClient baseUrl gameClient
  case res of
    Left err -> print err
    Right r -> pure ()
