{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Language.MiniZinc.Builder.Types
  ( MiniZincType(..)
  , demoteMiniZincType
  ) where

import Data.Singletons(fromSing, singByProxy, SingI)
import Data.Singletons.TH(genSingletons)

data MiniZincType = Bool
                  | Int
                  | String
                  | Array [MiniZincType] MiniZincType
                  | Set MiniZincType
  deriving(Show, Eq)

genSingletons [''MiniZincType]

demoteMiniZincType :: SingI a => proxy (a :: MiniZincType) -> MiniZincType
demoteMiniZincType = fromSing . singByProxy
