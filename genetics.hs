module Genetics where

import qualified Data.Vector as V
import Data.List (inits)
import Data.Fixed (mod')

type Chromasome = V.Vector Bool

roulettePick :: [(Chromasome, Double)] -> Double -> (Chromasome, Double)
roulettePick population value = (chromasome, score)
     where roulettePop = createRouletteProportions population
           normValue = value `mod'` 1.0
           (chromasome, score, _) = head $ dropWhile (\(_, _, x) -> x <= normValue) roulettePop

createRouletteProportions :: [(Chromasome, Double)] -> [(Chromasome, Double, Double)]
createRouletteProportions population =  zip3 chromasomes scores normTotals
     where chromasomes = fmap fst population
           scores = fmap (\(_, s) -> 1 / s) population
           runningTotals = fmap sum $ tail $ inits scores
           max = last runningTotals
           normTotals = fmap (\x -> x / max) runningTotals

crossover :: Int -> (Chromasome, Chromasome) -> (Chromasome, Chromasome)
crossover splitPoint (one, two) = (oneA V.++ twoB, twoA V.++ oneB)
      where (oneA, oneB) = V.splitAt splitPoint one
            (twoA, twoB) = V.splitAt splitPoint two

doMutation :: [Int] -> Chromasome -> Chromasome
doMutation bits chromasome = foldl mutate chromasome bits
      where mutate :: V.Vector Bool -> Int -> V.Vector Bool
            mutate empty _ = V.empty
            mutate c bitValue = front V.++ not (V.head rest) `V.cons` (V.tail rest)
                where moddedBitValue = bitValue `mod` V.length c
                      (front, rest) = V.splitAt moddedBitValue c
