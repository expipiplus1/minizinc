{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Language.MiniZinc
  ( MZ
  , MZT
  , runMZ
  , runMZT

  , parameter
  , boundedVar
  , constraint
  , solve
  , output

  , (/=:)
  , (==:)
  , show'

  , Expression
  , SolveType(..)

  , MiniZincType(..)
  ) where

import Control.Monad.Writer.Strict
import Data.DList as D
import GHC.Exts(IsList(..))
import Data.Function(on)
import Data.Functor.Identity(Identity, runIdentity)
import Data.String(IsString, fromString)
import Data.Text.Lazy(Text, pack)
import qualified Language.MiniZinc.Syntax as S

type MZT = WriterT (DList S.Item)

type MZ = MZT Identity

runMZ :: MZ () -> S.Model
runMZ = runIdentity . runMZT

runMZT :: Monad m => MZT m () -> m S.Model
runMZT mz =
  do (_, is) <- runWriterT mz
     pure (S.Model (D.toList is))

--
-- Functions for creating top level items
--

parameter :: Monad m => Text -> Expression a -> MZT m (Expression a)
parameter name value =
  do tellVarDecl (S.VarDecl (S.Type S.Par S.Int) name (Just (reifyExpression value)))
     pure (Var name)

boundedVar :: (Monad m, Range a)
           => Text -> Expression a -> Expression a -> MZT m (Expression a)
boundedVar name lb ub =
  do let type' = S.Type S.Var ((S.Bounded `on` reifyExpression) lb ub)
     tellVarDecl (S.VarDecl type' name Nothing)
     pure (Var name)

constraint :: Monad m => Expression 'Bool -> MZT m ()
constraint c = tellConstraint (S.Constraint (reifyExpression c))

solve :: Monad m => SolveType -> MZT m ()
solve Satisfy = tellSolve S.Satisfy
solve (Minimize e) = tellSolve (S.Minimize (reifyExpression e))
solve (Maximize e) = tellSolve (S.Maximize (reifyExpression e))

output :: Monad m => Expression ('Array 'String) -> MZT m ()
output o = tellOutput (S.Output (reifyExpression o))

--
-- The more typesafe version of the syntax
--

data MiniZincType = Bool
                  | Int
                  | String
                  | Array MiniZincType

class Lit a (b :: MiniZincType) | a -> b where
  reifyLit :: a -> S.Expr

instance Lit Int 'Int where
  reifyLit = S.LitInt . toInteger

instance Lit Bool 'Bool where
  reifyLit = S.LitBool

instance Lit Text 'String where
  reifyLit = S.LitString

instance Lit String 'String where
  reifyLit = S.LitString . pack

data Expression :: MiniZincType -> * where
  Lit   :: Lit a b => a -> Expression b
  Var   :: Text -> Expression a
  BinOp :: BinaryOperator a b c -> Expression a -> Expression b -> Expression c
  ArrayExpr :: [Expression a] -> Expression ('Array a)
  UnaryFunctionCall :: UnaryFunction a b -> Expression a -> Expression b

instance Num (Expression 'Int) where
  (+) = BinOp (BinaryOperator "+")
  (-) = BinOp (BinaryOperator "-")
  (*) = BinOp (BinaryOperator "*")
  negate = error "Expression negate"
  abs = error "Expression abs"
  signum = error "Expression signum"
  fromInteger i = Lit (fromInteger i :: Int)

instance IsString (Expression 'String) where
  fromString = Lit

instance IsList (Expression ('Array t)) where
  type Item (Expression ('Array t)) = Expression t
  fromList = ArrayExpr
  toList = error "toList Expression"

data BinaryOperator (a :: MiniZincType) (b :: MiniZincType) (c :: MiniZincType)
  = BinaryOperator Text

data UnaryFunction (a :: MiniZincType) (b :: MiniZincType)
  = UnaryFunction Text

class Optimizable (a :: MiniZincType)
instance Optimizable 'Int

class Range (a :: MiniZincType)
instance Range 'Int

data SolveType where
  Satisfy :: SolveType
  Minimize :: Optimizable a => Expression a -> SolveType
  Maximize :: Optimizable a => Expression a -> SolveType

--
-- Operators and functions in the language
--

infix 4 ==:, /=:
(/=:), (==:) :: Expression a -> Expression a -> Expression 'Bool
(/=:) = BinOp (BinaryOperator "!=")
(==:) = BinOp (BinaryOperator "==")

show' :: Expression a -> Expression 'String
show' = UnaryFunctionCall (UnaryFunction "show")

--
-- Converting it into the unsafe syntax version
--

reifyExpression :: Expression a -> S.Expr
reifyExpression (Lit l) = reifyLit l
reifyExpression (Var n) = S.Ident n
reifyExpression (BinOp (BinaryOperator op) l r) =
  S.BinOpExpr (S.binOpFromText op) (reifyExpression l) (reifyExpression r)
reifyExpression (ArrayExpr es) = S.ArrayExpr (reifyExpression <$> es)
reifyExpression (UnaryFunctionCall (UnaryFunction f) x) = S.CallExpr f [reifyExpression x]


--
-- Some utlilities for dealing with the monad
--

tellVarDecl :: Monad m => S.VarDecl -> MZT m ()
tellVarDecl v = tell (singleton (S.AVarDecl v))

tellConstraint :: Monad m => S.Constraint -> MZT m ()
tellConstraint c = tell (singleton (S.AConstraint c))

tellSolve :: Monad m => S.Solve -> MZT m ()
tellSolve s = tell (singleton (S.ASolve s))

tellOutput :: Monad m => S.Output -> MZT m ()
tellOutput o = tell (singleton (S.AnOutput o))

