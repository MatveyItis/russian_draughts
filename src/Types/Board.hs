-- | Доска

{-# LANGUAGE DeriveGeneric #-}

module Types.Board where

import Data.Aeson
import GHC.Generics

data Role =
  W | B | KW | KB
  -- ^ W - белая шашка
  -- ^ B - черная шашка
  -- ^ KW - белая дамка
  -- ^ KB - черная дамка
  deriving (Eq,Show,Read,Generic)

instance FromJSON Role
instance ToJSON Role

data Checker = Checker
  { x :: Int,
    y :: Int,
    role :: Role
  }
  deriving (Eq,Show,Read,Generic)
----- Вариант:
-- data Checker = W | B | KW | KB

mkChecker :: (Int, Int, Role) -> (Maybe Checker)
mkChecker (x, y, r)
  | 1 <= x && x <= 8 && 1 <= y && y <= 8 && mod (x + y) 2 == 0 = Just (Checker {x = x, y = y, role = r})

instance FromJSON Checker
instance ToJSON Checker

-- Доска, состоит из массива массивов 8х8
data Board = Board
  [[Maybe Checker]]
  deriving (Eq,Show,Read,Generic)

{-mkBoard :: [[Maybe Checker]] -> Board
mkBoard checkers
  |= Just (Board checkers)-}

instance FromJSON Board
instance ToJSON Board