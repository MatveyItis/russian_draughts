-- | Состояние игры и ходы

{-# LANGUAGE DeriveGeneric #-}

module Types.Game where

import Data.Aeson
import GHC.Generics
import Types.Board

data Move =
  Make (Int, Int, Int, Int)
  -- ^ сделать ход выбранной шашкой - первые (Int, Int)
  -- в нужную позицию - вторые (Int, Int)
  deriving (Eq,Show,Read,Generic)

instance FromJSON Move
instance ToJSON Move

data GameState = GameState
  { turn :: Int
    -- ^ очередь хода, 1 - белые, 0 - черные
    , board :: [[Maybe Checker]]
    -- ^ доска с расположением всех шашек
  } deriving (Eq,Show,Read,Generic)

instance FromJSON GameState
instance ToJSON GameState