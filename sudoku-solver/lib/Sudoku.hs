module Sudoku
  ( solve
  , parseGrid
  , gridToString
  , printGrid
  , serializeGrid
  , assign
  , digits
  , rows
  , columns
  , bounds
  , squares
  , units
  , allPossibilities
  , Square
  , Digit
  , Grid
  )
where

import Control.Exception
import Control.Monad
import Data.Array hiding (bounds)
import Data.List

-- Implementation is a Haskell port of the algorithm described at
-- https://norvig.com/sudoku.html.

type Digit  = Char
type Square = (Char, Char)
type Unit   = [Square]
type Grid   = Array Square [Digit]

digits  = "123456789"
rows    = "ABCDEFGHI"
columns = "123456789"

bounds :: (Square, Square)
bounds = (('A', '1'), ('I', '9'))

cross :: [a] -> [b] -> [(a, b)]
cross as bs = [(a, b) | a <- as, b <- bs]

squares :: [Square]
squares = cross rows columns

units :: [Unit]
units
  =  [cross rows [c]     | c <- columns]
  ++ [cross [r]  columns | r <- rows]
  ++ [cross rs   cs      | rs <- ["ABC", "DEF", "GHI"], cs <- ["123", "456", "789"]]

unitMap :: Array Square [Unit]
unitMap = array bounds [(s, [filter (/= s) u | u <- units, elem s u]) | s <- squares]

peerMap :: Array Square [Square]
peerMap = array bounds [(s, nub $ concat $ unitMap ! s) | s <- squares]

allPossibilities :: Grid
allPossibilities = array bounds [(s, digits) | s <- squares]

gridToString :: Grid -> String
gridToString grid =
  let width = 1 + (maximum $ map length $ elems grid)
      line  = concat $ intersperse "+" $ replicate 3 (replicate (width * 3) '-')
  in unlines $ do
    r <- rows
    let cell c    = center width $ grid ! (r, c)
    let cells     = concat [cell c ++ if c == '3' || c == '6' then "|" else "" | c <- columns]
    let seperator = if r == 'C' || r == 'F' then [line] else []
    [cells] ++ seperator

printGrid :: Grid -> IO ()
printGrid grid = putStrLn $ gridToString grid

serializeGrid :: Grid -> String
serializeGrid grid = map (cellToChar . (grid !)) squares
  where cellToChar cell = case cell of
          [digit] -> digit
          _       -> '.'

center :: Int -> String -> String
center n s =
  let total   = length s
      padding = (n - total) `div` 2
      extra   = if odd (n - total) then " " else ""
  in
    replicate padding ' ' ++ s ++ replicate padding ' ' ++ extra

parseGrid :: String -> Maybe Grid
parseGrid grid =
  foldM assign allPossibilities $ zip squares $ filter (\c -> elem c digits || c == '.' || c == '0') grid

assign :: Grid -> (Square, Digit) -> Maybe Grid
assign grid (square, digit) =
  if elem digit digits then do
    foldM eliminate grid $ zip (repeat square) $ delete digit (grid ! square)
  else
    pure grid

eliminate :: Grid -> (Square, Digit) -> Maybe Grid
eliminate grid (square, digit) =
  let cell = grid ! square in
    if elem digit cell then do
      let newCell = delete digit cell
      let newGrid = grid // [(square, newCell)]
      newGrid <- case newCell of
                   []     -> Nothing
                   [last] -> foldM eliminate newGrid $ zip (peerMap ! square) $ repeat last
                   _      -> Just newGrid
      foldM (locate digit) newGrid (unitMap ! square)
    else
      Just grid

locate :: Digit -> Grid -> Unit -> Maybe Grid
locate digit grid unit =
  case filter ((elem digit) . (grid !)) unit of
    []     -> Nothing
    [last] -> assign grid (last, digit)
    _      -> Just grid

search :: Grid -> Maybe Grid
search grid =
  case [(l, (square, cell)) | (square, cell) <- assocs grid, let l = length cell, l > 1] of
    []      -> Just grid
    options -> let (_, (square, cell)) = minimum options in
      msum [assign grid (square, digit) >>= search | digit <- cell]

solve :: String -> Maybe Grid
solve input = do
  grid <- parseGrid input
  search grid

  
