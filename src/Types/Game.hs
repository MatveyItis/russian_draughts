-- | Состояние игры и ходы

{-# LANGUAGE DeriveGeneric #-}

module Types.Game where

import Data.Aeson
import GHC.Generics
import Types.Board

data Move =
  Random
  -- ^ рандомный ход
  | Choose (Int, Int)
  -- ^ выбрать шашку, которой игрок хочет сходить
  | Make (Int, Int)
  -- ^ сделать ход выбранной на предыдущем шаге шашкой
  deriving (Eq,Show,Read,Generic)

instance FromJSON Move
instance ToJSON Move

data GameState = GameState
  { turn :: Int
    -- ^ очередь хода, 1 - белые, 0 - черные
    , chooseChecker :: Maybe Checker
    -- ^ выбранная игроком шашка
    , board :: [[Maybe Checker]]
    -- ^ доска с расположением всех шашек
  } deriving (Eq,Show,Read,Generic)

instance FromJSON GameState
instance ToJSON GameState