{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.Text()
import qualified Data.Text.Lazy.IO as T
import Language.MiniZinc
import Language.MiniZinc.Syntax(Model)
import Language.MiniZinc.Print(modelText)

default (Expression 'Int)

main :: IO ()
main = T.putStrLn (modelText aust)

aust :: Model
aust = runMZ $
  do nc <- parameter "nc" (3 :: Expression 'Int)
     wa  <- boundedVar "wa"  1 nc
     nt  <- boundedVar "nt"  1 nc
     sa  <- boundedVar "sa"  1 nc
     q   <- boundedVar "q"   1 nc
     nsw <- boundedVar "nsw" 1 nc
     v   <- boundedVar "v"   1 nc
     t   <- boundedVar "t"   1 nc
     constraint (wa  /=: nt)
     constraint (wa  /=: sa)
     constraint (nt  /=: sa)
     constraint (nt  /=: q)
     constraint (sa  /=: q)
     constraint (sa  /=: nsw)
     constraint (sa  /=: v)
     constraint (q   /=: nsw)
     constraint (nsw /=: v)
     solve Satisfy
     output [   "wa=",  show' wa
            , "\nnt=",  show' nt
            , "\nsa=",  show' sa
            , "\nq=",   show' q
            , "\nnsw=", show' nsw
            , "\nv=",   show' v
            , "\nt=" ,  show' t
            ]

