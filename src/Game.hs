-- | Функции для игры

module Game where

import Types.Board
import Types.Game

{--- | Применение хода к состоянию игры
applyMove :: GameState -- ^ старое состояние игры
          -> Move      -- ^ ход
          -> GameState -- ^ новое состояние игры
applyMove gs = do
  comb <- randomCombination
  applyMove gs $ Ask comb
applyMove gs (Ask c) = pure $ askCombination c gs
applyMove gs NoGuess = pure $ checkGuess Nothing gs
applyMove gs (Guess ans) = pure $ checkGuess (Just ans) gs-}

-- | Чистая функция для проверки комбинации
checkMake :: [Dot] -- ^ Предполагаемый ход
          -> GameState -- ^ Старое состояние игры
          -> GameState -- ^ Новое состояние игры
checkMake mans gs = gs
{-
  case turn gs of
    1 -> print ("White")
    0 -> print ("Black")
-}

  {-appendOldCombinations c $ clearAskedCombination $ case mans of
    Nothing -> gs { corrects = 0 }
    Just ans
      | ans == secretRule c -> gs { corrects = corrects gs + 1 }
      | otherwise -> gs { corrects = 0 }-}

-- | Начальное состояние игры
initialState :: GameState
initialState = GameState
  { turn = 1
  , checkers = [[mkChecker (1, 1, W)]]
  }

{-

gameProcess :: GameState -> IO ()


canPlay :: GameState -> Bool
canPlay getBoard = do
  let emptyCells = getEmptyCells grid
  let ln = length emptyCells
  ln > 0

getBoard :: GameState -> Board-}
