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
  , checkers = [[mkChecker (1, 1, W), Nothing, mkChecker (1, 3, W), Nothing, mkChecker (1, 5, W), Nothing, mkChecker (1, 7, W), Nothing],
                [Nothing, mkChecker (2, 2, W), Nothing, mkChecker (2, 4, W), Nothing, mkChecker (2, 6, W), Nothing, mkChecker (2, 8, W)],
                [mkChecker (3, 1, W), Nothing, mkChecker (3, 3, W), Nothing, mkChecker (3, 5, W), Nothing, mkChecker (3, 7, W), Nothing],
                [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                [Nothing, mkChecker (6, 2, B), Nothing, mkChecker (6, 4, B), Nothing, mkChecker (6, 6, B), Nothing, mkChecker (6, 8, B)],
                [mkChecker (7, 1, B), Nothing, mkChecker (7, 3, B), Nothing, mkChecker (7, 5, B), Nothing, mkChecker (7, 7, B), Nothing],
                [Nothing, mkChecker (8, 2, B), Nothing, mkChecker (8, 4, B), Nothing, mkChecker (8, 6, B), Nothing, mkChecker (8, 8, B)]]
  }

{-

gameProcess :: GameState -> IO ()


canPlay :: GameState -> Bool
canPlay getBoard = do
  let emptyCells = getEmptyCells grid
  let ln = length emptyCells
  ln > 0

getBoard :: GameState -> Board-}
