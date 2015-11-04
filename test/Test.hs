{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import Aust
import Control.Monad.Except(runExceptT)
import Language.MiniZinc
import Language.MiniZinc.Builder
import Test.Tasty.TH(defaultMainGenerator)
import Test.Tasty.HUnit

assertSat :: Model -> Assertion
assertSat m =
  do r <- runExceptT $ solveModel m
     case r of
       Left (SolveError err) -> assertFailure err
       Right Unsatisfiable -> assertFailure "Model is unsatisfiable"
       Right (Satisfiable _) -> pure ()

assertUnsat :: Model -> Assertion
assertUnsat m =
  do r <- runExceptT $ solveModel m
     case r of
       Left (SolveError err) -> assertFailure err
       Right Unsatisfiable -> pure ()
       Right (Satisfiable _) -> assertFailure "Model is satisfiable"

assertError :: Model -> Assertion
assertError m =
  do r <- runExceptT $ solveModel m
     case r of
       Left _ -> pure ()
       Right _ -> assertFailure "Trying to solve model didn't throw and error"

case_aust :: Assertion
case_aust = assertSat aust

case_unsat :: Assertion
case_unsat = assertUnsat . runMZ $
  do constraint false
     solve Satisfy

case_error :: Assertion
case_error = assertError . runMZ $ pure ()

main :: IO ()
main = $(defaultMainGenerator)


