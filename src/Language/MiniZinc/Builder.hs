module Language.MiniZinc.Builder
  ( MZ
  , MZT
  , runMZ
  , runMZT

  , parameter
  , boundedVar
  , boundedVarArray
  , rangeSet
  , constraint
  , solve
  , output

  , (/=:)
  , (==:)
  , (>:)
  , show'
  , (!)
  , to

  , true
  , false

  , Expression
  , SolveType(..)

  , MiniZincType(..)
  ) where

import Language.MiniZinc.Builder.Internal
