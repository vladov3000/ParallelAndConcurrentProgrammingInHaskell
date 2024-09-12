module Main where

import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Maybe
import System.Environment
import System.Exit
import Text.Printf

import Sudoku

data Mode
  = Serial
  | Parallel
  | ParallelMany

parseMode :: String -> IO Mode
parseMode input = case input of
  "serial"        -> pure Serial
  "parallel"      -> pure Parallel
  "parallel-many" -> pure ParallelMany
  _               -> do
    printf "Error: mode must be serial or parallel, got \"%s\".\n" input
    exitFailure

serial :: String -> [Maybe Grid]
serial file =
  let puzzles = lines file in
    map solve puzzles

parallel :: String -> [Maybe Grid]
parallel file =
  let
    puzzles  = lines file
    (as, bs) = splitAt (length puzzles `div` 2) puzzles
  in runEval $ do
    as <- rpar $ force $ map solve as
    bs <- rpar $ force $ map solve bs
    rseq as
    rseq bs
    pure $ as ++ bs

parallelMany :: String -> [Maybe Grid]
parallelMany file =
  let puzzles = lines file in
    runEval $ parMap' solve puzzles

parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' f xs =
  case xs of
    [] -> pure []
    x : xs -> do
      y  <- rpar $ f x
      ys <- parMap' f xs
      pure $ y : ys

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mode, path] -> do
      mode <- parseMode mode
      file <- readFile path
      let command = case mode of
            Serial       -> serial
            Parallel     -> parallel
            ParallelMany -> parallelMany
      print $ length $ filter isJust $ command file
    _ -> do
      putStrLn "Error: Expected exactly 2 arguments.\nUsage: mode path"
      exitFailure
