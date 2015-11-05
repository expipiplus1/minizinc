{-# LANGUAGE OverloadedStrings #-}

module Language.MiniZinc.Syntax where

import Data.Text.Lazy(Text)
import Data.Monoid((<>))

data Model = Model [Item]

data Item = AVarDecl VarDecl
          | AConstraint Constraint
          | ASolve Solve
          | AnOutput Output

data VarDecl = VarDecl Type Text (Maybe Expr)

data Constraint = Constraint Expr

data Solve = Satisfy
           | Minimize Expr
           | Maximize Expr

data Output = -- | the output expression must be an array of strings
              Output Expr

data Type = Type VarOrPar BaseType

data VarOrPar = Var
              | Par

data BaseType = Bool
              | Int
              | Float
              | String
              | Ann
                -- | The indices used must be either Int or Bounded Ints and the
                -- element type must not be another array type
              | Array [BaseType] Type
              | Opt BaseType
              | Bounded Expr Expr

data Expr = Ident Text
          | LitInt Integer
          | LitBool Bool
          | LitString Text
          | ArrayExpr [Expr]
          | ArrayIndex Expr [Expr]
          | BinOpExpr BinOp Expr Expr
          | UnOpExpr UnOp Expr
          | CallExpr Text [Expr]

data BinOp = BiImplication
           | Implies
           | ImpliedBy
           | Disjunction
           | ExclusiveOr
           | Conjunction
           | LessThan
           | GreaterThan
           | LessThanEquals
           | GreaterThanEquals
           | Equals
           | NEquals
           | In
           | Subset
           | Superset
           | Union
           | Diff
           | SymDiff
           | Range
           | Intersect
           | Concat
           | Plus
           | Minus
           | Times
           | Divide
           | Div
           | Mod
           | IdentOp Text

-- | binOpInfo returns the text representation and precedence of binary
-- operators according to section 7.3.2 of
-- http://www.minizinc.org/2.0/doc-lib/minizinc-spec.pdf
binOpInfo :: BinOp -> (Text, Int)
binOpInfo BiImplication     = ("↔"       , 1200)
binOpInfo Implies           = ("→"       , 1100)
binOpInfo ImpliedBy         = ("←"       , 1100)
binOpInfo Disjunction       = ("∨"       , 1000)
binOpInfo ExclusiveOr       = ("xor"     , 1000)
binOpInfo Conjunction       = ("∧"       , 900)
binOpInfo LessThan          = ("<"       , 800)
binOpInfo GreaterThan       = (">"       , 800)
binOpInfo LessThanEquals    = ("≤"       , 800)
binOpInfo GreaterThanEquals = ("≥"       , 800)
binOpInfo Equals            = ("=="      , 800)
binOpInfo NEquals           = ("≠"       , 800)
binOpInfo In                = ("∈"       , 700)
binOpInfo Subset            = ("⊆"       , 700)
binOpInfo Superset          = ("⊇"       , 700)
binOpInfo Union             = ("∪"       , 600)
binOpInfo Diff              = ("diff"    , 600)
binOpInfo SymDiff           = ("symdiff" , 600)
binOpInfo Range             = (".."      , 500)
binOpInfo Intersect         = ("∩"       , 300)
binOpInfo Concat            = ("++"      , 200)
binOpInfo Plus              = ("+"       , 400)
binOpInfo Minus             = ("-"       , 400)
binOpInfo Times             = ("*"       , 300)
binOpInfo Divide            = ("/"       , 300)
binOpInfo Div               = ("div"     , 300)
binOpInfo Mod               = ("mod"     , 300)
binOpInfo (IdentOp i)       = ("`" <> i <> "`", 100)

opPrecedence :: BinOp -> Int
opPrecedence = snd . binOpInfo

data UnOp = Positive
          | Negative
          | Not

unOpName :: UnOp -> Text
unOpName Positive = "+"
unOpName Negative = "-"
unOpName Not = "¬"

