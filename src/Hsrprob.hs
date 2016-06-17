module Hsrprob where
--    ( someFunc
--    ) where

import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V
import GHC.Float(float2Double)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

someFunc :: IO ()
someFunc = putStrLn "someFunc"

toGlitch :: Int -> Int
toGlitch 1 = 1
toGlitch _ = 0

toSuccess :: Int -> Int
toSuccess x = if x >= 5 then 1 else 0

type Vec = V.Vector Float

v1glitch :: Vec
v1glitch = V.fromList [5/6, 1/6]

v1succ :: Vec
v1succ = V.fromList [4/6, 2/6]

fglitcher :: Vec -> Vec
fglitcher o =
  let v1 = V.map (* (5/6)) o `V.snoc` 0
      v2 = V.map (* (1/6)) $ V.cons 0 o
  in V.zipWith (+) v1 v2

fcglitcher :: Vec -> Vec
fcglitcher o =
  let v1 = V.map (* (3/6)) o `V.snoc` 0
      v2 = V.map (* (1/6)) $ V.cons 0 o
  in V.zipWith (+) v1 v2

fsuccer :: Vec -> Vec
fsuccer o =
  let v1 = V.map (* (4/6)) o `V.snoc` 0
      v2 = V.map (* (2/6)) $ V.cons 0 o
  in V.zipWith (+) v1 v2


type VV = VB.Vector Vec

maxDice :: Int
maxDice = 15

glitches, cglitches :: VV
glitches = V.singleton 0 `VB.cons` VB.iterateN maxDice fglitcher v1glitch
cglitches = V.singleton 0 `VB.cons` VB.iterateN maxDice fcglitcher v1glitch

nglitches = VB.map V.head $ VB.tail glitches

successes :: VV
successes = V.singleton 0 `VB.cons` VB.iterateN maxDice fsuccer v1succ


glitchProbs :: [Float]
glitchProbs = map glitchProb $ VB.toList glitches

cglitchProbs :: [Float]
cglitchProbs = map glitchProb $ VB.toList cglitches

glitchProb :: Vec -> Float
glitchProb v =
  let l = V.length v
      c = 1 + ((l-1) `div` 2)
  in V.sum $ V.drop c v

zips1 :: [x] -> [(Int,x)]
zips1 = tail . zip [0..]

glitchGraph = toFile def "glitch_prob.png" $ do
    layout_title .= "Probability to glitch"
    setColors [opaque blue, opaque red]
    plot (line "glitch" [take 10 $ zips1 glitchProbs])
    plot (line "critical glitch" [take 10 $ zips1 cglitchProbs])
