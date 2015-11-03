{-# LANGUAGE OverloadedStrings #-}

module Language.MiniZinc.Syntax where

import Data.Text.Lazy(Text)

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
          | BinOpExpr BinOp Expr Expr
          | UnOpExpr UnOp Expr
          | CallExpr Text [Expr]

data BinOp = Add
           | Subtract
           | Multiply
           | NEq

binOpFromText :: Text -> BinOp
binOpFromText "+" = Add
binOpFromText "-" = Subtract
binOpFromText "*" = Multiply
binOpFromText "!=" = NEq
binOpFromText _ = error "binOpFromText"

-- | opPrecedence returns the precedence according to section 7.3.2 of
-- http://www.minizinc.org/2.0/doc-lib/minizinc-spec.pdf
opPrecedence :: BinOp -> Int
opPrecedence Add = 400
opPrecedence Subtract = 400
opPrecedence Multiply = 300
opPrecedence NEq = 800

data UnOp = Positive
          | Negative
