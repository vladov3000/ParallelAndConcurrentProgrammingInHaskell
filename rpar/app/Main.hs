module Main where

import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import System.Environment
import System.Exit
import Text.Read
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  case args of
    [testIndex, x, y] -> do
      testIndex <- parseTestIndex testIndex
      x <- parseInt x
      y <- parseInt y
      
      let test = tests !! testIndex
      startTime <- getCurrentTime
      r <- evaluate $ runEval $ test fibonacci x y
      
      printTimeSince startTime
      print r
      printTimeSince startTime
    _ -> do
      putStrLn "Error: Expected exactly 3 arguments.\nUsage: rpar TEST_INDEX X Y"
      exitFailure

parseInt :: String -> IO Int
parseInt arg = case readMaybe arg of
    Just arg -> pure arg
    Nothing  -> do
      printf "Error: %s is not an integer.\n" arg
      exitFailure

parseTestIndex :: String -> IO Int
parseTestIndex arg = do
  index <- parseInt arg
  if 0 <= index && index < testsLength then
    pure index
  else do
    printf "Error: %d is not between 0 and %d.\n" index testsLength
    exitFailure

printTimeSince :: UTCTime -> IO ()
printTimeSince start = do
  now <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime now start) :: Double)

type Test = (Int -> Int) -> Int -> Int -> Eval (Int, Int)

tests :: [Test]
tests = [bothParallel, parallelThenSequential, parallelJoin, sequential]

testsLength :: Int
testsLength = length tests

-- This should print 0 then max(time(a), time(b))
-- because rpar causes us to immediately return and kick
-- these computations off in parallel.
bothParallel :: Test
bothParallel f x y = do
  a <- rpar $ f x
  b <- rpar $ f y
  pure (a, b)

-- This should print time(b) then max(time(a), time(b)).
-- The rpar is kicked off in parallel, but rseq must be evaluated
-- before we can return a result.
parallelThenSequential :: Test
parallelThenSequential f x y = do
  a <- rpar $ f x
  b <- rseq $ f y
  pure (a, b)

-- This should print max(time(a), time(b)) two times.
-- We compute a in parallel to b and then wait for it once b is done.
-- This is effectively "join"ing our parallel result back to the Eval.
parallelJoin :: Test
parallelJoin f x y = do
  a <- rpar $ f x
  b <- rseq $ f y
  rseq a
  pure (a, b)

sequential :: Test
sequential f x y = do
  a <- rseq $ f x
  b <- rseq $ f y
  pure (a, b)

-- Purposely bad implementation of fibonacci.
fibonacci :: Int -> Int
fibonacci n = if n < 2 then 1 else fibonacci (n - 1) + fibonacci (n - 2)
