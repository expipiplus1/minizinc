{-# LANGUAGE FlexibleContexts #-}

module Language.MiniZinc
  ( solveModel
  , SolveResult(..)
  , SolveError(..)
  , Model
  ) where

import Control.Monad.IO.Class(MonadIO, liftIO)
import Control.Monad.Except(MonadError, throwError)
import qualified Data.Text.Lazy.IO as T (hPutStr)
import Language.MiniZinc.Syntax(Model)
import Language.MiniZinc.Print(modelText)
import System.Exit(ExitCode(..))
import System.IO(hClose)
import System.IO.Temp(withSystemTempFile)
import System.Process(CreateProcess(..), CmdSpec(..), StdStream(..)
                     , readCreateProcessWithExitCode)

data SolveError = SolveError String

data SolveResult = Unsatisfiable
                 | Satisfiable String
  deriving (Show, Eq)

solveModel :: (MonadError SolveError m, MonadIO m) => Model -> m SolveResult
solveModel m =
  do let s = modelText m
     (ec, out, err) <- liftIO $ withSystemTempFile ".mzn" (\filepath handle ->
       do T.hPutStr handle s
          hClose handle
          let process = CreateProcess { cmdspec = ShellCommand ("mzn-g12fd " ++
                                                                filepath)
                                      , cwd = Nothing
                                      , env = Nothing
                                      , std_in = Inherit
                                      , std_out = CreatePipe
                                      , std_err = Inherit
                                      , close_fds = True
                                      , create_group = False
                                      , delegate_ctlc = False
                                      }
          readCreateProcessWithExitCode process "")
     case ec of
       ExitSuccess ->
         do let ls = lines out
            case ls of
              ("=====UNSATISFIABLE=====":_) -> pure Unsatisfiable
              _ -> pure (Satisfiable out)
       ExitFailure _ -> let msg = "Error running MiniZinc. stderr:\n" ++ err
                        in throwError (SolveError msg)


