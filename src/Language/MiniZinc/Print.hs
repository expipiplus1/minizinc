{-# LANGUAGE OverloadedStrings #-}

module Language.MiniZinc.Print
  ( modelText
  ) where

import Data.List(intersperse)
import Data.String(fromString)
import Data.Text.Lazy(Text)
import Language.MiniZinc.Syntax
import Text.PrettyPrint.Leijen.Text((<>), (<+>), text, vcat, Doc, displayT,
                                    renderCompact, integer, parens, brackets)

-- | $modelText$ returns MiniZinc source code representing the given model
modelText :: Model -> Text
modelText = displayT . renderCompact . modelDoc

modelDoc :: Model -> Doc
modelDoc (Model items) = vcat (itemDoc <$> items)

itemDoc :: Item -> Doc
itemDoc (AVarDecl v) = varDeclDoc v
itemDoc (AConstraint c) = constraintDoc c
itemDoc (ASolve s) = solveDoc s
itemDoc (AnOutput o) = outputDoc o

varDeclDoc :: VarDecl -> Doc
varDeclDoc (VarDecl type' name Nothing) = typeDoc type' <> ":" <+>
                                          text name <> ";"
varDeclDoc (VarDecl type' name (Just e)) = typeDoc type' <> ":" <+>
                                           text name <+>
                                           "=" <+> exprDoc e <> ";"

constraintDoc :: Constraint -> Doc
constraintDoc (Constraint c) = "constraint" <+> exprDoc c <> ";"

solveDoc :: Solve -> Doc
solveDoc Satisfy = "solve satisfy;"
solveDoc (Minimize e) = "solve minimize" <+> exprDoc e <> ";"
solveDoc (Maximize e) = "solve maximize" <+> exprDoc e <> ";"

outputDoc :: Output -> Doc
outputDoc (Output o) = "output" <+> exprDoc o <> ";"

typeDoc :: Type -> Doc
typeDoc (Type vp baseType) = varOrParDoc vp <+> baseTypeDoc baseType

varOrParDoc :: VarOrPar -> Doc
varOrParDoc Var = "var"
varOrParDoc Par = ""

baseTypeDoc :: BaseType -> Doc
baseTypeDoc Bool = "bool"
baseTypeDoc Int = "int"
baseTypeDoc Float = "float"
baseTypeDoc String = "string"
baseTypeDoc Ann = "ann"
baseTypeDoc (Array indices t) =
  "array" <> brackets (commaSeparated (baseTypeDoc <$> indices)) <+>
  "of" <+> typeDoc t
baseTypeDoc (Opt baseType) = "opt" <+> baseTypeDoc baseType
baseTypeDoc (Bounded lb ub) = exprDoc lb <> ".." <> exprDoc ub
baseTypeDoc (Set t) = "set of" <+> baseTypeDoc t

exprDoc :: Expr -> Doc
exprDoc = exprPrecDoc maxBound

-- | $exprPrecDoc p expr$ renders expr as though it were in a context of
-- precedence p
exprPrecDoc :: Int -> Expr -> Doc
exprPrecDoc _ (Ident n) = text n
exprPrecDoc _ (LitInt i) = integer i
exprPrecDoc _ (LitBool b) = if b then "true" else "false"
exprPrecDoc _ (LitString s) = fromString (show s)
exprPrecDoc _ (ArrayExpr es) = brackets $ commaSeparated (exprDoc <$> es)
exprPrecDoc _ (ArrayIndex a is) = exprPrecDoc 0 a <>
                                  brackets (commaSeparated (exprDoc <$> is))
exprPrecDoc p (BinOpExpr op l r) = let p' = opPrecedence op
                                       s = exprPrecDoc p' l <+> binOpDoc op <+>
                                           exprPrecDoc p' r
                                   in if p' >= p then parens s else s
exprPrecDoc _ (UnOpExpr op e) = unOpDoc op <> exprPrecDoc 0 e
exprPrecDoc _ (CallExpr f es) = text f <>
                                parens (commaSeparated (exprDoc <$> es))
exprPrecDoc _ (ArrayComp e gs f) = "[" <> exprDoc e <+> "|"
                                   <+> commaSeparated (generatorDoc <$> gs)
                                   <+> "where" <+> exprDoc f <> "]"

commaSeparated :: [Doc] -> Doc
commaSeparated = mconcat . intersperse ", "

binOpDoc :: BinOp -> Doc
binOpDoc = text . fst . binOpInfo

unOpDoc :: UnOp -> Doc
unOpDoc = text . unOpName

generatorDoc :: Generator -> Doc
generatorDoc (InSet is e) = commaSeparated (text <$> is) <+> "in" <+> exprDoc e

