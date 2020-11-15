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

type GameMonad = ReaderT (TMVar GameState) Handler

type GameApi = "field" :> Get '[JSON] GameState
  :<|> "field" :> "checkers" :> Get '[JSON] [[Maybe Checker]]
  :<|> "field" :> ReqBody '[JSON] Move :> Post '[JSON] GameState

fieldGet :: GameMonad GameState
fieldGet = do
  var <- ask
  state <- liftIO $ atomically $ readTMVar var
  pure state

fieldCheckers :: GameMonad [[Maybe Checker]]
fieldCheckers = do
  var <- ask
  state <- liftIO $ atomically $ readTMVar var
  let brd = checkers state
  pure brd

fieldPost :: Move -> GameMonad GameState
fieldPost move = do
  var <- ask
  liftIO $ atomically $ do
    state <- takeTMVar var
    let state' = case move of
          Make dots -> checkMake dots state
    putTMVar var state'
    pure state'

gameServer :: TMVar GameState -> Server GameApi
gameServer var = hoistServer gameApi gameToHandler $
                 fieldGet :<|> fieldCheckers :<|> fieldPost
  where gameToHandler :: GameMonad a -> Handler a
        gameToHandler act = runReaderT act var

gameApi :: Proxy GameApi
gameApi = Proxy

getField :: ClientM GameState
getCheckers :: ClientM [[Maybe Checker]]
postMove :: Move -> ClientM GameState
getField :<|> getCheckers :<|> postMove = client gameApi

runClient :: BaseUrl -> ClientM a -> IO (Either ClientError a)
runClient baseUrl actions = do
  mgr <- newManager defaultManagerSettings
  let env = mkClientEnv mgr baseUrl
  runClientM actions env