module Genetics where

import System.Random
import Control.Monad

type Chromasome = [Bool]

crossover :: Int -> (Chromasome, Chromasome) -> (Chromasome, Chromasome)
crossover splitPoint (one, two) = (oneA ++ twoB, twoA ++ oneB)
      where (oneA, oneB) = splitAt splitPoint one
            (twoA, twoB) = splitAt splitPoint two

doMutation :: [Int] -> Chromasome -> Chromasome
doMutation bits chromasome = foldl mutate chromasome bits
      where mutate [] _ = []
            mutate chromasome bitValue = front ++ not flipBit : rest
                where moddedBitValue = bitValue `mod` length chromasome
                      (front, flipBit : rest) = splitAt moddedBitValue chromasome

