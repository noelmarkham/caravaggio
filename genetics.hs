{-# LANGUAGE NoMonomorphismRestriction #-}

module Genetics where

import System.Random
import Control.Monad

crossover :: ([Bool], [Bool]) -> IO [Bool]
crossover (first, second) = do
                        place <- randomRIO (0, length first)
                        return $ fst ( splitAt place first) ++ snd ( splitAt place second)

maybeSwapBit :: Bool -> IO Bool
maybeSwapBit b = do
             rand <- randomRIO (0, 100) :: IO Int
             return $ if (rand == 1) then (not b) else b

newMutate :: [Bool] -> IO [Bool]
newMutate bools = mapM maybeSwapBit bools

mutate :: [Bool] -> IO [Bool]
mutate bools = do
          bitsToFlip <- replicateM (quot (length bools) 10) $ randomRIO (0, length bools)
          return $ doMutation bools bitsToFlip

doMutation :: [Bool] -> [Int] -> [Bool]
doMutation bools bitsToFlip = foldl foldFunction bools bitsToFlip

foldFunction :: [Bool] -> Int -> [Bool]
foldFunction bools bit = f ++ (not $ head s) : (tail s)
                 where (f, s) = splitAt bit bools

