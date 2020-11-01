module Lib
  where

import Types
import Game
import Api

import Servant (serve)
import Network.Wai (Application)

app :: Application
app = serve gameApi gameServer
