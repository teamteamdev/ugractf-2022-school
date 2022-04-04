{-# OPTIONS_GHC -fno-warn-orphans #-}

module TicTacToe.Game where

import GHC.Generics (Generic)
import Data.Semigroup
import Data.List
import Data.Maybe
import Data.Ord
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Monad.State
import qualified Data.List.NonEmpty as NE
import Data.Vector (Vector)
import qualified Data.Vector as V

import TicTacToe.Types

instance (Hashable a) => Hashable (Vector a) where
  hashWithSalt salt = hashWithSalt salt . V.toList

data XOCell = CellEmpty
            | CellX
            | CellO
            deriving (Eq, Generic, Hashable)

instance Show XOCell where
  show CellEmpty = "_"
  show CellX = "X"
  show CellO = "O"

instance Semigroup XOCell where
  CellX <> CellX = CellX
  CellO <> CellO = CellO
  _ <> _ = CellEmpty

sideToCell :: GameSide -> XOCell
sideToCell SideX = CellX
sideToCell SideO = CellO

data XOField = XOField { xoField :: Vector (Vector XOCell) }
             deriving (Eq, Generic, Hashable)

instance Show XOField where
  show field = intercalate "\n" $ map showLine $ V.toList $ xoField field
    where showLine = foldMap show

emptyXOField :: XOField
emptyXOField = XOField (V.fromList [line, line, line])
  where line = V.fromList [CellEmpty, CellEmpty, CellEmpty]

data XOState = XOInProgress XOField
             | XOFinished (Maybe GameSide)
             | XOIllegalTurn
             deriving (Show, Eq)

placeSymbol :: Int -> Int -> GameSide -> XOField -> XOState
placeSymbol x y side field
  | x < 0 || x >= 3 || y < 0 || y >= 3 = XOIllegalTurn
  | curCell /= CellEmpty = XOIllegalTurn
  | otherwise =
    case gameFinished newField of
      Nothing -> XOInProgress newField
      Just winner -> XOFinished winner
  where curLine = xoField field V.! y
        curCell = curLine V.! x
        newLine = curLine V.// [(x, sideToCell side)]
        newField = XOField $ xoField field V.// [(y, newLine)]

gameFinished :: XOField -> Maybe (Maybe GameSide)
gameFinished field =
  case find (/= CellEmpty) $ map checkWinner winnerLines of
    Just CellX -> Just (Just SideX)
    Just CellO -> Just (Just SideO)
    _
      | V.all (V.all (/= CellEmpty)) $ xoField field -> Just Nothing
      | otherwise -> Nothing
  where getCell x y = (xoField field V.! y) V.! x
        winnerLines =
          [ [(0, y), (1, y), (2, y)] | y <- [0..2] ] ++
          [ [(x, 0), (x, 1), (x, 2)] | x <- [0..2] ] ++
          [ [ (0, 0), (1, 1), (2, 2) ]
          , [ (2, 0), (1, 1), (0, 2) ]
          ]
        checkWinner line = sconcat $ NE.fromList $ map (uncurry getCell) line

type Move = (Int, Int)
type Score = Int
type StateMove a = State (HashMap (XOField, GameSide) (Maybe (Score, Move))) a

bestMove :: GameSide -> XOField -> Maybe (Score, Move)
bestMove mySide field0 = evalState (makeTurn mySide field0) HM.empty
  where makeTurn :: GameSide -> XOField -> StateMove (Maybe (Score, Move))
        makeTurn side field = do
          saved <- get
          case HM.lookup (field, side) saved of
            Just ret -> return ret
            Nothing -> do
              scores <- mapM moveScore emptyFields
              let sumScore = sum $ map fst scores
                  ret = fmap ((sumScore, ) . snd) $ listToMaybe $ reverse $ sortBy (comparing fst) scores
              modify' $ HM.insert (field, side) ret
              return ret

          where getCell x y = (xoField field V.! y) V.! x
                emptyFields = [(x, y) | x <- [0..2], y <- [0..2], getCell x y == CellEmpty]

                moveScore :: Move -> StateMove (Score, Move)
                moveScore point@(x, y) = do
                  let newState = placeSymbol x y side field
                  score <-
                    case newState of
                      XOFinished Nothing -> return 0
                      XOFinished (Just winSide) -> return $ if winSide == mySide then 1 else -1
                      XOInProgress newField -> do
                        (totalScore, _bestTurn) <- fromJust <$> makeTurn (oppositeSide side) newField
                        return totalScore
                      XOIllegalTurn -> error "Impossible"
                  return (score, point)
