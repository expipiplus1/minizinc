{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Aust
  ( aust
  ) where

import Language.MiniZinc.Builder
import Language.MiniZinc.Syntax(Model)

aust :: Model
aust = runMZ $
  do nc <- parameter (3 :: Expression 'Int)
     wa  <- boundedVar 1 nc
     nt  <- boundedVar 1 nc
     sa  <- boundedVar 1 nc
     q   <- boundedVar 1 nc
     nsw <- boundedVar 1 nc
     v   <- boundedVar 1 nc
     t   <- boundedVar 1 nc
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

