module Main where

import Control.Exception
import Control.Monad
import Data.Array
import Data.Array.IO
import Data.List
import Data.List.Split
import Data.Time.Clock
import System.Random
import Text.Printf

import Sudoku

infinity :: Double
infinity = 1 / 0

solveAll :: [String] -> String -> Double -> IO ()
solveAll grids name showThreshold = do
  (times, results) <- unzip <$> traverse (timeSolve showThreshold) grids
  let n = length grids
  when (n > 1) $ do
    let format      = "Solved %d of %d %s puzzles (avg %.2f secs (%d Hz), max %.2f secs).\n"
        solvedCount = sum $ map fromEnum results
        averageTime = (sum times) / (fromIntegral n)
        throughput  = floor $ (fromIntegral n) / (sum times) :: Int
        maxTime     = maximum times
    printf format solvedCount n name averageTime throughput maxTime

timeSolve :: Double -> String -> IO (Double, Bool)
timeSolve showThreshold grid = do
   start  <- getCurrentTime
   values <- evaluate $ solve grid
   end    <- getCurrentTime
   let time = realToFrac (diffUTCTime end start) :: Double
   when (time > showThreshold) $ do
     case parseGrid grid of
       Just grid -> printGrid grid
       Nothing   -> pure ()
     case values of
       Just values -> printGrid values
       Nothing     -> pure ()
     printf "(%.2f seconds)\n" time
   pure (time, solved values)

solved :: Maybe Grid -> Bool
solved grid =
  case grid of
    Just grid -> all (unitSolved grid) units
    Nothing   -> False
  where unitSolved grid unit =
          let inUnit = [grid ! square | square <- unit] in
            all ((== 1) . length) inUnit && sort (concat inUnit) == digits

fromFile :: String -> String -> IO [String]
fromFile path seperator = do
  contents <- readFile path
  pure $ filter (not . null) $ splitOn seperator contents

randomPuzzle :: Int -> IO String
randomPuzzle n = do
  shuffled  <- shuffle squares
  grid      <- go allPossibilities shuffled
  case grid of
    Just grid -> pure $ serializeGrid grid
    Nothing   -> randomPuzzle n
  where
     go grid squares =
       case squares of
         square : squares -> do
           chosen <- randomChoice (grid ! square)
           case assign grid (square, chosen) of
             Just grid ->
               let cells = [cell | s <- squares, let cell = grid ! s, length cell == 1] in
                 if length cells >= n && length (nub cells) >= 8 then
                   pure $ Just grid
                 else
                   go grid squares
             Nothing -> pure Nothing
         [] -> pure Nothing

shuffle :: [a] -> IO [a]
shuffle xs = do
  a <- newArray xs
  forM [1..n] $ \i -> do
    j  <- randomRIO (i, n)
    xi <- readArray a i
    xj <- readArray a j
    writeArray a j xi
    pure xj
  where
    n = length xs
    newArray :: [a] -> IO (IOArray Int a)
    newArray xs = newListArray (1, n) xs

randomChoice :: [a] -> IO a
randomChoice xs = do
  index <- randomRIO (0, length xs - 1)
  pure $ xs !! index

grid1 = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
grid2 = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
hard1 = ".....6....59.....82....8....45........3........6..3.54...325..6.................."

main :: IO ()
main = do
  solveAll [grid1, grid2, hard1] "hardcoded" 0.0
  fromFile "puzzles/easy50.txt"  "========"  >>= \grids -> solveAll grids "easy"    infinity
  fromFile "puzzles/top95.txt"   "\n"        >>= \grids -> solveAll grids "hard"    infinity
  fromFile "puzzles/hardest.txt" "\n"        >>= \grids -> solveAll grids "hardest" infinity
  sequence (replicate 100 $ randomPuzzle 17) >>= \grids -> solveAll grids "random"  100
