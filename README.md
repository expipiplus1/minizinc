# MiniZinc

[MiniZinc][mz] syntax ADTs and a safe builder for MiniZinc source in Haskell.

This targets [version 2.0 of MiniZinc][spec].

## Current status

There's plenty of MiniZinc constructs not implemented yet; this includes

- Annotations
- Custom functions
- Sets
- Different solvers (currently only uses mzn-g12fd)

Please create an issue if there's some feature you want which isn't
implemented, or better yet submit a pull request.

## Usage

Here's a transcription of the "aust" example from section 1 of the [MiniZinc
tutorial][tute]

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Aust(aust) where

import Language.MiniZinc.Builder
import Language.MiniZinc.Syntax(Model)

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
```

## Contact information

Contributions and bug reports are welcome!

Please feel free to contact me on GitHub or as "jophish" on freenode.

-Joe

[mz]: http://www.minizinc.org/
[spec]: http://www.minizinc.org/2.0/doc-lib/minizinc-spec.pdf
[tute]: http://www.minizinc.org/downloads/doc-latest/minizinc-tute.pdf
