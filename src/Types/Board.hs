-- | Доска

{-# LANGUAGE DeriveGeneric #-}

module Types.Board
 (
  Checker,
  mkChecker,
  Board
 ) where

import Data.Aeson
import GHC.Generics

data Checker = Checker
  { x :: Int,
    y :: Int,
    role :: [Char]
  }
  deriving (Eq,Show,Read,Generic)
----- Вариант:
-- data Checker = W | B | KW | KB

mkChecker :: (Int, Int, [Char]) -> (Maybe Checker)
mkChecker (x, y, r)
  | 1 <= x && x <= 8 && 1 <= y && y <= 8 = Just (Checker {x = x, y = y, role = r})

instance FromJSON Checker
instance ToJSON Checker

-- todo пока не работает конструктор
-- Доска, состоит из массива массивов 8х8
data Board = Board
  [[Maybe Checker]]
  deriving (Eq,Show,Read,Generic)

{-
mkBoard :: [[Checker]] -> Board
mkBoard (checkers)
  |= Just (Board checkers)
-}

instance FromJSON Board
instance ToJSON Board