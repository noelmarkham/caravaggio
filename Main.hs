{-# LANGUAGE BangPatterns #-}
module Main where

import Genetics
import GeneticImage
import Diagrams.Prelude 
import Diagrams.Backend.Cairo
import Text.Printf (printf)
import System.Random (randomRIO)
import Data.List (sortBy)
import Control.Monad (replicateM, zipWithM)
import Data.Function (on)
import Diagrams.Backend.Cairo.List
import Control.Concurrent (forkIO)
import Data.Vector (fromList)
import qualified Data.Set as S

populationSize :: Int
populationSize = 20

ioMutate :: Int -> Chromasome -> IO Chromasome
ioMutate chromasomeLength chromasome = do
                  bits <- replicateM (quot chromasomeLength 10) $ randomRIO (0, chromasomeLength)
                  return $ doMutation bits chromasome

maybeCrossover :: Int -> Chromasome -> Chromasome -> IO (Chromasome, Chromasome)
maybeCrossover chromasomeLength c1 c2 = do
                  randSelect <- randomRIO (0, 1.0) :: IO Double
                  randPos <- randomRIO (0, chromasomeLength)
                  return $ if (randSelect < 0.7) then crossover randPos (c1, c2) else (c1, c2)

listOfTupToList :: [(a, a)] -> [a]
listOfTupToList l = concat $ map tupToList l
         where tupToList (a, b) = a : b : []

newPopulation :: [(Chromasome, Double)] -> IO [Chromasome]
newPopulation population = do
                  rand1 <- replicateM (quot populationSize 2) $ randomRIO (0, 1.0)
                  rand2 <- replicateM (quot populationSize 2) $ randomRIO (0, 1.0)
                  let pickForPop = fst . roulettePick population
                  let randPick1 = fmap pickForPop rand1
                  let randPick2 = fmap pickForPop rand2
                  let crossFunc = maybeCrossover bitCount
                  let mutateFunc = ioMutate bitCount
                  newPop <- zipWithM crossFunc randPick1 randPick2
                  let flatNewPop = listOfTupToList newPop
                  let s = S.fromList flatNewPop
                  putStrLn $ "New Pop uniques a: " ++ show (S.size s)
                  --mapM mutateFunc flatNewPop
                  return flatNewPop

iteration :: (Diagram Cairo R2 -> IO Double) -> Int -> [Chromasome] -> IO (Int, [Chromasome])
iteration compareFunction count population = do
                  putStrLn $ "Iteration " ++ show count
                  -- generate pictures for all of the chromasomes
                  let pictures = fmap generateImage population

                  -- get scores for all of the pictures
                  scores <- mapM compareFunction pictures

                  -- get the best picture and write it to disk
                  let bestDiagram = fst $ head $ sortBy (compare `on` snd) $ zip pictures scores

                  let filename = printf "/vagrant/caravaggio/target/out/%010d.png" count
                  persistImage filename 400 400 bestDiagram

                  -- create a new population using the genetics functions
                  newPop <- newPopulation $ zip population scores

                  let s = S.fromList newPop

                  putStrLn $ "New Pop uniques b: " ++ show (S.size s)

                  let next = count + 1
                  if (next < 1000000) then iteration compareFunction next newPop else return (next, newPop)

randomChromasome :: IO Chromasome
randomChromasome = do
        bits <- replicateM bitCount $ randomRIO (False, True)
        return $ fromList bits

run :: Diagram Cairo R2 -> IO ()
run baseImg = do
        pop <- replicateM populationSize randomChromasome
        iteration (imageValue baseImg) 1 pop
        return ()

main = do
  imageEither <- fromDisk "/vagrant/svg/target/MonaLisa.png"
  case imageEither of
             Left error -> putStrLn $ "ERROR: " ++ error
             Right img -> run img
