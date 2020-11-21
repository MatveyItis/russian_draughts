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
  {
    role :: Role
  }
  deriving (Eq,Show,Read,Generic)
----- Вариант:
-- data Checker = W | B | KW | KB

mkChecker :: (Role) -> (Maybe Checker)
mkChecker (r) = Just (Checker {role = r})

instance FromJSON Checker
instance ToJSON Checker