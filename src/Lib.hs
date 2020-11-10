module Lib
  where

import Types
import Game
import Api

import Control.Concurrent.STM
import Servant (serve)
import Network.Wai (Application)

mkApp :: IO Application
mkApp = do
  var <- atomically $ newTMVar initialState
  pure $ serve gameApi $ gameServer var