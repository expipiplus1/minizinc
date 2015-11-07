{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.MiniZinc.Builder.Internal
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

import Control.Monad.RWS.Strict
import Data.DList as D
import Data.HList(HList(..))
import GHC.Exts(IsList(..), Constraint)
import Data.Function(on)
import Data.Functor.Identity(Identity, runIdentity)
import Data.Singletons(SingI)
import Data.String(IsString, fromString)
import Data.Text.Lazy(Text, pack)
import qualified Language.MiniZinc.Syntax as S
import Language.MiniZinc.Builder.Types

--
-- The monad for constructing minizinc source
--

type MZT = RWST () (DList S.Item) MZState

type MZ = MZT Identity

runMZ :: MZ () -> S.Model
runMZ = runIdentity . runMZT

runMZT :: Monad m => MZT m () -> m S.Model
runMZT mz =
  do (_, is) <- execRWST mz () initialMZState
     pure (S.Model (D.toList is))

data MZState = MZState {nextId :: Int}

initialMZState :: MZState
initialMZState = MZState {nextId = 0}

getName :: Monad m => MZT m Text
getName =
  do i <- gets nextId
     modify' (\s -> s {nextId = i+1})
     pure ("v" <> pack (show i))

--
-- Functions for creating top level items
--

parameter :: SingI a => Monad m => Expression a -> MZT m (Expression a)
parameter value =
  do name <- getName
     let type' = reifyType (demoteMiniZincType value)
     tellVarDecl (S.VarDecl (S.Type S.Par type') name (Just (reifyExpression value)))
     pure (Var name)

boundedVar :: (Monad m, Range a)
              => Expression a -> Expression a -> MZT m (Expression a)
boundedVar lb ub =
  do name <- getName
     let type' = S.Type S.Var ((S.Bounded `on` reifyExpression) lb ub)
     tellVarDecl (S.VarDecl type' name Nothing)
     pure (Var name)

boundedVarArray :: (Monad m, Range i, Range e)
         => (Expression i, Expression i) -> (Expression e, Expression e)
         -> MZT m (Expression ('Array '[i] e))
boundedVarArray (ilb, iub) (elb, eub) =
  do name <- getName
     let elemType = S.Type S.Var ((S.Bounded `on` reifyExpression) elb eub)
         indexType = (S.Bounded `on` reifyExpression) ilb iub
         type' = S.Type S.Par (S.Array [indexType] elemType)
     tellVarDecl (S.VarDecl type' name Nothing)
     pure (Var name)

rangeSet :: Monad m
            => Expression 'Int -> Expression 'Int -> MZT m (Expression ('Set 'Int))
rangeSet lb ub =
  do name <- getName
     let type' = S.Type S.Par (S.Set S.Int)
         r = reifyExpression (lb `to` ub)
     tellVarDecl (S.VarDecl type' name (Just r))
     pure (Var name)


constraint :: Monad m => Expression 'Bool -> MZT m ()
constraint c = tellConstraint (S.Constraint (reifyExpression c))

solve :: Monad m => SolveType -> MZT m ()
solve Satisfy = tellSolve S.Satisfy
solve (Minimize e) = tellSolve (S.Minimize (reifyExpression e))
solve (Maximize e) = tellSolve (S.Maximize (reifyExpression e))

output :: Monad m => Expression ('Array '[i] 'String) -> MZT m ()
output o = tellOutput (S.Output (reifyExpression o))

--
-- The more typesafe version of the syntax
--

data Function (c :: Constraint) (arguments :: [MiniZincType])
              (ret :: MiniZincType) = Function Text

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
  Lit :: Lit a b => a -> Expression b
  Var :: Text -> Expression a
  Arr :: [Expression a] -> Expression ('Array i a)
  App :: (ReifyHList (HList (Map Expression as)), c)
         => Function c as r -> HList (Map Expression as) -> Expression r
  ArrIndex :: (ReifyHList (HList (Map Expression is)), EveryElement Index is)
              => Expression ('Array is e) -> HList (Map Expression is)
              -> Expression e

instance Num (Expression 'Int) where
  (+) = call (Function "'+'" :: '[ 'Int, 'Int] --> 'Int)
  (-) = call (Function "'-'" :: '[ 'Int, 'Int] --> 'Int)
  (*) = call (Function "'*'" :: '[ 'Int, 'Int] --> 'Int)
  negate = error "Expression negate"
  abs = error "Expression abs"
  signum = error "Expression signum"
  fromInteger i = Lit (fromInteger i :: Int)

instance IsString (Expression 'String) where
  fromString = Lit

instance IsList (Expression ('Array '[i] t)) where
  type Item (Expression ('Array '[i] t)) = Expression t
  fromList = Arr
  toList = error "toList Expression"

class Optimizable (a :: MiniZincType)
instance Optimizable 'Int

class Range (a :: MiniZincType)
instance Range 'Int

class Index (a :: MiniZincType)
instance Index 'Int

data SolveType where
  Satisfy :: SolveType
  Minimize :: Optimizable a => Expression a -> SolveType
  Maximize :: Optimizable a => Expression a -> SolveType

--
-- Operators and functions in the language
--

infix 4 ==:, /=:, >:
(/=:), (==:), (>:) :: Expression a -> Expression a -> Expression 'Bool
(/=:) = call (Function "'!='" :: '[a,a] --> 'Bool)
(==:) = call (Function "'=='" :: '[a,a] --> 'Bool)
(>:) = call (Function "'>'" :: '[a,a] --> 'Bool)

show' :: Range a => Expression a -> Expression 'String
show' = call (Function "show" :: '[a] --> 'String)

true, false :: Expression 'Bool
true = Lit True
false = Lit False

infixl 9 !

(!) :: (HCall (Map Expression is), ReifyHList (HList (Map Expression is)),
        EveryElement Index is)
       => Expression ('Array is e)
       -> ExpandFunction (Map Expression is) (Expression e)
(!) = index

to :: (Range a) => Expression a -> Expression a -> Expression ('Set a)
to = call (Function "'..'" :: Range a ==> '[a,a] --> 'Set a)

--
-- Converting it into the unsafe syntax version
--

reifyType :: MiniZincType -> S.BaseType
reifyType t = case t of
                Bool -> S.Bool
                Int -> S.Int
                String -> S.String
                Array ts e -> S.Array (reifyType <$> ts) (S.Type S.Var (reifyType e)) -- Wrong, need to encode var/par information
                Set e -> S.Set (reifyType e)

reifyExpression :: Expression a -> S.Expr
reifyExpression (Lit l) = reifyLit l
reifyExpression (Var n) = S.Ident n
reifyExpression (Arr es) = S.ArrayExpr (reifyExpression <$> es)
reifyExpression (App (Function f) es) = S.CallExpr f (reifyHExpressions es)
reifyExpression (ArrIndex a is) = S.ArrayIndex (reifyExpression a)
                                               (reifyHExpressions is)

class ReifyHList a where
  reifyHExpressions :: a -> [S.Expr]

instance ReifyHList (HList '[]) where
  reifyHExpressions HNil = []

instance ReifyHList (HList as) => ReifyHList (HList (Expression a ': as)) where
  reifyHExpressions (HCons x xs) = reifyExpression x : reifyHExpressions xs

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

--
-- Some type tricks to make calling functions a little nicer
--

infix 1 -->
infixr 0 ==>

type as --> r = Function () as r

type family (==>) (c :: Constraint) (f :: *) :: * where
  c ==> Function c' as r = Function (c, c') as r

call :: (HCall (Map Expression as), ReifyHList (HList (Map Expression as)), c)
        => Function c as r -> ExpandFunction (Map Expression as) (Expression r)
call f = hCall (App f)

index :: (HCall (Map Expression is), ReifyHList (HList (Map Expression is)),
          EveryElement Index is)
         => Expression ('Array is e)
         -> ExpandFunction (Map Expression is) (Expression e)
index a = hCall (ArrIndex a)

class HCall as where
  hCall :: (HList as -> r) -> ExpandFunction as r

instance HCall '[] where
  hCall f = f HNil

instance HCall as => HCall (a ': as) where
  hCall f x = hCall (\xs -> f (x `HCons` xs))

type family Map (f :: a -> b) (xs :: [a]) :: [b] where
  Map f '[] = '[]
  Map f (x ': xs) = f x ': Map f xs

type family ExpandFunction (arguments :: [*]) (ret :: *) :: * where
  ExpandFunction '[] r = r
  ExpandFunction (a ': as) r = a -> ExpandFunction as r

type family EveryElement (c :: a -> Constraint) (as :: [a]) :: Constraint where
  EveryElement c '[] = ()
  EveryElement c (a ': as) = (c a, EveryElement c as)
