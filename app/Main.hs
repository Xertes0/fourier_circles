module Main where

import Data.Complex
import Text.Printf

data PointsData = PointsData
  { points :: [(Double, Double)]
  , step :: Double
  } deriving (Show)

pointToComplex :: (Double, Double) -> Complex Double
pointToComplex (x, y) = x :+ y

complexToPoints :: Complex Double -> (Double, Double)
complexToPoints val = (realPart val, imagPart val)

stringToPair :: String -> (Double, Double)
stringToPair str = (read a, read $ tail b)
  where
    (a, b) = break (== ' ') str

readPoints :: FilePath -> IO PointsData
readPoints path =
  (\content ->
     PointsData
       {points = map stringToPair $ tail content, step = read $ head content})
    . lines
    <$> readFile path

getC :: PointsData -> Int -> Complex Double
getC pData 0 =
  sum [expected i * (stepNorm :+ 0) | i <- [0 .. length (points pData) - 1]]
  where
    expected i = pointToComplex $ points pData !! i
    stepNorm :: Double
    stepNorm = 1.0 / fromIntegral (length $ points pData)
getC pData n = sum [f i | i <- [0 .. length (points pData) - 1]]
  where
    f i =
      expected
        * exp ((((-fromIntegral n) * 2 * pi * tNorm) :+ 0) * (0 :+ 1))
        * (stepNorm :+ 0)
      where
        expected = pointToComplex $ points pData !! i
        tNorm :: Double
        tNorm = fromIntegral i / fromIntegral (length $ points pData)
        stepNorm :: Double
        stepNorm = 1.0 / fromIntegral (length $ points pData)

type CCache = [Complex Double]

cCache :: CCache -> Int -> Complex Double
cCache cache n = cache !! (((length cache - 1) `div` 2) + n)

buildCCache :: PointsData -> Int -> CCache
buildCCache pData count = [getC pData n | n <- [-count .. count]]

getVector :: PointsData -> Complex Double -> Int -> Int -> Complex Double
getVector pData c n i =
  c * exp (((fromIntegral n * 2 * pi * tNorm) :+ 0) * (0 :+ 1))
  where
    -- c = getC pData n
    tNorm :: Double
    tNorm = fromIntegral i / fromIntegral (length $ points pData)

getPoints :: PointsData -> Int -> [Complex Double]
getPoints pData count = [f i | i <- [0 .. length (points pData) - 1]]
  where
    cache = buildCCache pData count
    f i = sum ([getVector pData (cCache cache n) n i | n <- [-count .. count]]) * (stepNorm :+ 0)
    stepNorm :: Double
    stepNorm = 1.0 / fromIntegral (length $ points pData)

main :: IO ()
main = do
  pData <- readPoints "./helpers/points.txt"
  mapM_ (printPoint . complexToPoints) $ getPoints pData 50
  where
    printPoint (x, y) = printf "%s %s\n" (show x) (show y)
