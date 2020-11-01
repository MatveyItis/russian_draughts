-- | Описание API

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( module Api
  , BaseUrl (..)
  , Scheme (..)
  , ClientM
  ) where

import Types
import Game

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Random
import Control.Monad.Random
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Servant

type GameMonad g = ReaderT (TMVar GameState) (RandT g Handler)

type GameApi = "field" :> Get '[JSON] GameState
  :<|> "field" :> "board" :> Get '[JSON] [(Checker, Int)]
  :<|> "field" :> ReqBody '[JSON] Move :> Post '[JSON] GameState

fieldGet :: RandomGen g => GameMonad g GameState
fieldGet = do
  var <- ask
  state <- liftIO $ atomically $ readTMVar var
  pure state

gameServer :: TMVar GameState -> Server GameApi
gameServer var = hoistServer gameApi gameToHandler $
                 fieldGet
  where gameToHandler :: GameMonad StdGen a -> Handler a
        gameToHandler act = do
          g <- liftIO getStdGen
          evalRandT (runReaderT act var) g

gameApi :: Proxy GameApi
gameApi = Proxy

getField :: ClientM GameState
getField = client gameApi

runClient :: BaseUrl -> ClientM a -> IO (Either ClientError a)
runClient baseUrl actions = do
  mgr <- newManager defaultManagerSettings
  let env = mkClientEnv mgr baseUrl
  runClientM actions env