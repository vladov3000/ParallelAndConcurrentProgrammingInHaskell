module Main where

import Control.Monad
import Data.Array
import Data.Function
import Data.List
import Data.Random.Normal
import Data.Vector (Vector)
import Graphics.Rendering.Chart.Easy ((.=))
import System.Environment
import System.Exit
import System.Random
import Text.Printf
import Text.Read (readMaybe)

import qualified Data.Vector                            as Vector
import qualified Data.Vector.Mutable                    as MVector
import qualified Graphics.Rendering.Chart.Easy          as Chart
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo

data Point = Point !Double !Double
  deriving (Show, Eq)

zeroPoint :: Point
zeroPoint = Point 0 0

squareDistance :: Point -> Point -> Double
squareDistance (Point x y) (Point x' y') = (x - x') ** 2 + (y - y') ** 2

pointToTuple :: Point -> (Double, Double)
pointToTuple (Point x y) = (x, y)

data Cluster = Cluster
  { clusterId     :: Int
  , clusterCenter :: Point }
  deriving (Show, Eq)

data PointSum = PointSum !Int !Double !Double

addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum n xs ys) (Point x y) = PointSum (n + 1) (xs + x) (ys + y)

pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster i (PointSum count xs ys) =
  Cluster i $ Point (xs / count') (ys / count')
  where count' = fromIntegral count

makeCluster :: Int -> [Point] -> Cluster
makeCluster i points = pointSumToCluster i $ foldl' addToPointSum (PointSum 0 0 0) points

assign :: Int -> [Cluster] -> [Point] -> Vector PointSum
assign nclusters clusters points = Vector.create $ do
  result <- MVector.replicate nclusters $ PointSum 0 0 0
  let nearestCluster test = fst $ Data.List.minimumBy (compare `on` snd) distances
        where distances = [(c, squareDistance test $ clusterCenter c) | c <- clusters]
  let addpoint point = do
        let i = clusterId $ nearestCluster point
        pointSum <- MVector.read result i
        MVector.write result i  $! (addToPointSum pointSum point)
        pure result
  Prelude.mapM_ addpoint points
  pure result

makeNewClusters :: Vector PointSum -> [Cluster]
makeNewClusters pointSums =
  [pointSumToCluster i pointSum | (i, pointSum@(PointSum count _ _)) <- pointSums', count > 0]
  where pointSums' = zip [0..] $ Vector.toList pointSums

step :: Int -> [Cluster] -> [Point] -> [Cluster]
step nclusters clusters points = makeNewClusters $ assign nclusters clusters points

tooMany :: Int
tooMany = 1000

kmeansSeq :: Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeansSeq nclusters points startClusters = do
  let loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters =
        if n > tooMany then do
          putStrLn "Giving up."
          pure clusters
        else do
          printf "Iteration %d\n" n
          putStr $ unlines $ map show clusters
          let clusters' = step nclusters clusters points
          if clusters' == clusters
            then pure clusters
            else loop (n + 1) clusters'
    in loop 0 startClusters

generatePoints :: Int -> Double -> Double -> Double -> Double -> IO [Point]
generatePoints count meanX meanY stdDevX stdDevY = do
  gen <- newStdGen
  let (genX, genY) = split gen
      xs = normals' (meanX, stdDevX) genX
      ys = normals' (meanY, stdDevY) genY
  pure $ zipWith Point xs $ take count ys

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [clustersArg, minSamples, maxSamples] -> do
      nClusters   <- parseInt clustersArg
      minSamples' <- parseInt minSamples
      maxSamples' <- parseInt maxSamples
      
      samples <- replicateM nClusters $ randomRIO (minSamples', maxSamples')
      xs      <- replicateM nClusters $ randomRIO (-10, 10)
      ys      <- replicateM nClusters $ randomRIO (-10, 10)
      stdDevs <- replicateM nClusters $ randomRIO (1.5, 2)
      
      let parameters = zip5 samples xs ys stdDevs stdDevs
      pointsByCluster <- mapM (\(a, b, c, d, e) -> generatePoints a b c d e) parameters
      let points = concat pointsByCluster

      gen <- newStdGen
      let randomClusters  = randomRs (0, nClusters - 1) gen
      let clusterToPoints = accumArray (flip (:)) [] (0, nClusters - 1) $ zip randomClusters points
      let clusters        = map (uncurry makeCluster) $ assocs clusterToPoints

      finalClusters <- kmeansSeq nClusters points clusters
      print finalClusters

      Cairo.toFile Chart.def "result.png" $ do
            Chart.layout_title .= "K-means Clustering"
            Chart.plot $ Chart.points "Points" $ map pointToTuple points
            Chart.plot $ Chart.points "Initial Clusters" $ map (pointToTuple . clusterCenter) clusters
            Chart.plot $ Chart.points "Final Clusters" $ map (pointToTuple . clusterCenter) finalClusters
            
    _ -> do
      printf "Error: expected exactly 3 arguments.\nUsage: kmeans CLUSTERS MIN_SAMPLES MAX_SAMPLES\n"
      exitFailure

parseInt :: String -> IO Int
parseInt arg = case readMaybe arg of
    Just arg' -> pure arg'
    Nothing  -> do
      printf "Error: %s is not an integer.\n" arg
      exitFailure
