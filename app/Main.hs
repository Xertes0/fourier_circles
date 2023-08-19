module Main where

import Data.Complex
import Data.List
import Graphics.Gloss
import GHC.Float

data PointsData = PointsData
  { points :: [(Double, Double)]
  , step :: Double
  } deriving (Show)

pointToComplex :: (Double, Double) -> Complex Double
pointToComplex (x, y) = x :+ y

complexToPoint :: Complex Double -> (Double, Double)
complexToPoint val = (realPart val, imagPart val)

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

getPoints :: PointsData -> CCache -> Int -> [Complex Double]
getPoints pData cache count = [f i | i <- [0 .. length (points pData) - 1]]
  where
    f i = sum ([getVector pData (cCache cache n) n i | n <- [-count .. count]])-- * (stepNorm :+ 0)
    -- stepNorm :: Double
    -- stepNorm = 1.0 / fromIntegral (length $ points pData)

-- Actual count is this * 2 + 1
circleCount :: Int
circleCount = 10

arrow :: Picture
arrow =
  pictures [line [(0, 0), (0, 1)], line [(0.15, 0.75), (0, 1), (-0.15, 0.75)]]

vecAngle :: (Float, Float) -> Float
vecAngle (x, y) = atan2 x y * 180 / pi

doAnimate :: PointsData -> [(Float, Float)] -> CCache -> Float -> Picture
doAnimate pData ps cache time = pictures (circles ++ arrows ++ [pointsPic])
  where
    i = min (round (time * 50)) $ length (points pData)
    pointsPic = line $ take (i + 1) ps
    buildVec n = ((vecx', vecy'), vecLength)
      where
        (vecx, vecy) = complexToPoint $ getVector pData (cCache cache n) n i
        (vecx', vecy') = (double2Float vecx, double2Float vecy)
        vecLength = sqrt $ (vecx' ** 2) + (vecy' ** 2)
    vecs = [buildVec n | n <- [-circleCount .. circleCount]]
    vecs' = sortBy (\(_, a) (_, b) -> compare b a) vecs
    posVecs = scanl (\(a2, b2) ((a1, b1), _) -> (a1 + a2, b1 + b2)) (0, 0) vecs'
    circles =
      tail
        $ zipWith
            (\(_, len) (x, y) ->
               color (makeColor 0 0 0 0.25) $ translate x y $ circle len)
            vecs'
            posVecs
    arrows =
      tail
        $ zipWith
            (\((x1, y1), len) (x2, y2) ->
               color (makeColor 0 0 0 0.5) $ translate x2 y2 $ scale len len $ rotate (vecAngle (x1, y1)) arrow) -- line [(x2, y2), (x1 + x2, y1 + y2)])
            vecs'
            posVecs

main :: IO ()
main = do
  pData <- readPoints "./helpers/points.txt"
  let cache = buildCCache pData circleCount
  let ps =
        (\(x, y) -> (double2Float x, double2Float y))
          -- . (\(x, y) -> (x * 150, y * 150))
          . complexToPoint
          <$> getPoints pData cache circleCount
  animate
    (InWindow "Fourier series" (1280, 1000) (100, 100))
    white
    (doAnimate pData ps cache)
  -- mapM_ (printPoint . complexToPoint) $ getPoints pData 50
  -- where
  --   printPoint (x, y) = printf "%s %s\n" (show x) (show y)
