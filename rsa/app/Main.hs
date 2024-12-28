module Main where

import Control.Parallel.Strategies
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List.Split (splitOn)
import System.Environment
import System.Exit
import System.Random
import Text.Printf
import Text.Read

import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    ["makeKeys"] -> do
      r <- abs <$> randomIO
      s <- abs <$> randomIO
      let (n, d, e) = makeKeys r s
      printf "Public key:  %d,%d\n" n d
      printf "Private key: %d,%d\n" n e
    ["encrypt", privateKey, inputPath] -> do
      input <- readInput inputPath
      (n, e) <- parseKey privateKey
      B.putStr $ encrypt n e input
    ["decrypt", publicKey, inputPath] -> do
      input <- readInput inputPath
      (n, d) <- parseKey publicKey
      case decrypt n d input of
        Just output -> B.putStr output
        Nothing     -> do
          putStrLn "Error: invalid input."
          exitFailure
    _ -> do
      putStrLn "Error: Invalid arguments."
      putStrLn "Usage:"
      putStrLn "  rsa makeKeys"
      putStrLn "  rsa encrypt PRIVATE_KEY INPUT"
      putStrLn "  rsa decrypt PUBLIC_KEY  INPUT"
      exitFailure

readInput :: String -> IO ByteString
readInput inputPath = case inputPath of
  "-" -> B.getContents 
  _   -> B.readFile inputPath

makeKeys :: Integer -> Integer -> (Integer, Integer, Integer)
makeKeys r s = (p * q, d, invert ((p - 1) * (q - 1)) d)
  where p = nextPrime r
        q = nextPrime s
        d = nextPrime (p + q + 1)

nextPrime :: Integer -> Integer
nextPrime a = case filter isPrime $ iterate (+2) (if even a then a + 1 else a) of
                result : _ -> result
                []         -> error "There must have been at least 1 prime found."

-- Fermat primality test. Unreliable.
isPrime :: Integer -> Bool
isPrime p = and [power (p - 1) p x == 1 | x <- [3, 5, 7]]

power :: Integer -> Integer -> Integer -> Integer
power 0 _ _             = 1
power n m x | even n    = square (power (n `div` 2) m x) `mod` m
            | otherwise = (x * power (n - 1) m x) `mod` m

square :: Integer -> Integer
square x = x * x

invert :: Integer -> Integer -> Integer
invert n a = if e < 0 then e + n else e
  where e            = loop n 0 a 1
        loop _ v 0 _ = v
        loop g v h w = loop h w (g `mod` h) (v - (g `div` h) * w)

parseKey :: String -> IO (Integer, Integer)
parseKey input =
  case splitOn "," input of
    [n, d] -> liftA2 (,) (parseInteger n) (parseInteger d)
    _      -> do
      printf "Error: key \"%s\" must contain exactly one comma.\n" input
      exitFailure

parseInteger :: String -> IO Integer
parseInteger input =
  case readMaybe input of
    Just input' -> pure input'
    Nothing     -> do
      printf "Error: %s is not an integer.\n" input
      exitFailure

encrypt :: Integer -> Integer -> ByteString -> ByteString
encrypt n e = B.unlines
  . withStrategy (parBuffer 100 rdeepseq)
  . map (B.pack . show . power e n . code) 
  . chunk (size n)

chunk :: Int -> ByteString -> [ByteString]
chunk n s = if B.null s 
  then []
  else as : chunk n bs
    where (as, bs) = B.splitAt (fromIntegral n) s

size :: Integer -> Int
size n = (length (show n) * 47) `div` 100 -- log_128 10 = 0.4745

decrypt :: Integer -> Integer -> ByteString -> Maybe ByteString
decrypt n d input = do
  chunks <- mapM B.readInteger $ B.lines input
  pure $ B.concat $ map (B.pack . decode . power d n . fst) chunks

code :: ByteString -> Integer
code = B.foldl' (\acc c -> 128 * acc + fromIntegral (fromEnum c)) 0

decode :: Integer -> String
decode n = reverse (expand n)
  where expand 0 = []
        expand x = toEnum (fromIntegral (x `mod` 128)) : expand (x `div` 128)
