module Genetics where

import Data.List (inits)
import Data.Fixed (mod')

type Chromasome = [Bool]

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
           normTotals = fmap (\x -> x / max ) runningTotals

crossover :: Int -> (Chromasome, Chromasome) -> (Chromasome, Chromasome)
crossover splitPoint (one, two) = (oneA ++ twoB, twoA ++ oneB)
      where (oneA, oneB) = splitAt splitPoint one
            (twoA, twoB) = splitAt splitPoint two

doMutation :: [Int] -> Chromasome -> Chromasome
doMutation bits chromasome = foldl mutate chromasome bits
      where mutate [] _ = []
            mutate c bitValue = front ++ not flipBit : rest
                where moddedBitValue = bitValue `mod` length c
                      (front, flipBit : rest) = splitAt moddedBitValue c

