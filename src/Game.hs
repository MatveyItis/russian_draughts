-- | Функции для игры

module Game where

import Types.Board
import Types.Game

{-applyMove :: GameState -- ^ старое состояние игры
          -> Move      -- ^ ход
          -> GameState -- ^ новое состояние игры
applyMove gs (Make dots) = pure $ makeMove dots gs-}

makeMove :: [Dot]
         -> GameState
         -> GameState
makeMove dots gs = gs{-do
  let checkers = checkers gs
  let dot1 = dots[0]
  let dot2 = dots[1]
  let x1 = (x dot1) - 1
  let y1 = (y dot1) - 1
  let x2 = (x dot2) - 1
  let y2 = (y dot2) - 1
  let optCheckerX = checkers[x1]
  let optChecker = optCheckerX[y1]
  if isNothing optChecker then
    checkers[x1][y1] = Nothing
    checkers[x2][y2] = optChecker
    gs = gs {checkers = checkers, turn = 0}
  else gs
-}
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
  , checkers = [[mkChecker (W), Nothing, mkChecker (W), Nothing, mkChecker (W), Nothing, mkChecker (W), Nothing],
                [Nothing, mkChecker (W), Nothing, mkChecker (W), Nothing, mkChecker (W), Nothing, mkChecker (W)],
                [mkChecker (W), Nothing, mkChecker (W), Nothing, mkChecker (W), Nothing, mkChecker (W), Nothing],
                [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
                [Nothing, mkChecker (B), Nothing, mkChecker (B), Nothing, mkChecker (B), Nothing, mkChecker (B)],
                [mkChecker (B), Nothing, mkChecker (B), Nothing, mkChecker (B), Nothing, mkChecker (B), Nothing],
                [Nothing, mkChecker (B), Nothing, mkChecker (B), Nothing, mkChecker (B), Nothing, mkChecker (B)]]
  }

{-

gameProcess :: GameState -> IO ()


canPlay :: GameState -> Bool
canPlay getBoard = do
  let emptyCells = getEmptyCells grid
  let ln = length emptyCells
  ln > 0

getBoard :: GameState -> Board-}
