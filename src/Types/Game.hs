-- | Состояние игры и ходы

{-# LANGUAGE DeriveGeneric #-}

module Types.Game where

import Data.Aeson
import GHC.Generics
import Types.Board

data Dot = Dot
  { a :: Int,
    -- ^ значение точки по горизонтали
    b :: Int
    -- ^ значение точки по вертикали
  } deriving (Eq,Show,Read,Generic)

instance FromJSON Dot
instance ToJSON Dot

data Move = Make [Dot]
  -- ^ сделать ход выбранной шашкой в нужную позицию(ии)
  deriving (Eq,Show,Read,Generic)

instance FromJSON Move
instance ToJSON Move

data GameState = GameState
  { turn :: Int
    -- ^ очередь хода, 1 - белые, 0 - черные
    , board :: Board
    -- ^ доска с расположением всех шашек
  } deriving (Eq,Show,Read,Generic)

instance FromJSON GameState
instance ToJSON GameState

-- | Сдвинуть выбранную шашку
moveChecker :: Move -- ^ Текущий ход
            -> GameState   -- ^ Старое состояние игры
            -> GameState   -- ^ Новое состояние
moveChecker c gs = gs
--todo обход двумерного массива ???

-- | Поменять ход
changeTurn :: GameState -- ^ Старое состояние игры
           -> GameState -- ^ Новое состояние игры
changeTurn gs = if (turn gs == 1) then gs {turn = 0} else gs {turn = 1}
