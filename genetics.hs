module Genetics where

type Chromasome = [Bool]

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

