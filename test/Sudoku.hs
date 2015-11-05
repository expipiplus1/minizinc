{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}

module Sudoku
  ( sudokuModel
  , linearSudoku
  ) where

import Data.Array(Array, bounds)
import qualified Data.Array as A
import Language.MiniZinc.Builder
import Language.MiniZinc.Syntax(Model)

linearSudoku :: Array Int (Maybe Int) -> Model
linearSudoku p = runMZ $
  do let n' = snd (bounds p)
     n <- parameter (fromIntegral n' :: Expression 'Int)
     puzzle <- boundedVarArray (1, n) (1, n)
     sequence_ [constraint (puzzle ! fromIntegral i /=: puzzle ! fromIntegral j)
               | i <- [1..n']
               , j <- [i+1..n']]
     sequence_ [constraint (puzzle ! fromIntegral i ==: fromIntegral v)
               | i <- [1..n']
               , Just v <- pure $ p A.! i]
     solve Satisfy

sudokuModel :: Array (Int, Int) (Maybe Int) -> Model
sudokuModel _ = runMZ $ solve Satisfy
