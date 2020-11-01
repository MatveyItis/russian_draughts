-- | Функции для игры

module Game where

import Types.Board
import Types.Game

-- | Применение хода к состоянию игры
{-
applyMove :: RandomGen g
          => GameState -- ^ старое состояние игры
          -> Move      -- ^ ход
          -> Rand g GameState -- ^ новое состояние игры
applyMove gs Random = do
  comb <- randomCombination
  applyMove gs $ Ask comb
applyMove gs (Ask c) = pure $ askCombination c gs
applyMove gs NoGuess = pure $ checkGuess Nothing gs
applyMove gs (Guess ans) = pure $ checkGuess (Just ans) gs
-}

-- | Чистая функция для проверки комбинации
{-
checkGuess :: Maybe Int -- ^ Предполагаемый ответ
           -> GameState -- ^ Старое состояние игры
           -> GameState -- ^ Новое состояние игры
checkGuess mans gs = case askedCombination gs of
  Nothing -> gs
  Just c -> appendOldCombinations c $ clearAskedCombination $ case mans of
    Nothing -> gs { corrects = 0 }
    Just ans
      | ans == secretRule c -> gs { corrects = corrects gs + 1 }
      | otherwise -> gs { corrects = 0 }
-}

-- | Начальное состояние игры
initialState :: GameState
initialState = GameState
  { turn = 1
  , chooseChecker = Nothing
  , board = [[mkChecker (1, 1, "W")]]
  }
