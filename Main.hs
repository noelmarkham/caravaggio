module Main where

import Genetics
import GeneticImage
import Diagrams.Prelude 
import Diagrams.Backend.Cairo
import Text.Printf (printf)
import System.Random (randomRIO)
import Data.List (sortBy)
import Control.Monad (replicateM)
import Data.Function (on)
import Diagrams.Backend.Cairo.List

doGenetics :: [(Chromasome, Double)] -> IO [Chromasome]
doGenetics population = do
          let roulette = roulettePick population
          r1s <- replicateM 5 $ randomRIO (0, 1.0)
          r2s <- replicateM 5 $ randomRIO (0, 1.0)
          let r1picks = fmap (fst . roulette) r1s
          let r2picks = fmap (fst . roulette) r2s
          let pairs = zip r1picks r2picks
          geneticsListOfLists <- mapM geneticOperation pairs
          return $ concat geneticsListOfLists ++ fmap fst population

geneticOperation :: (Chromasome, Chromasome) -> IO [Chromasome]
geneticOperation (c1, c2) = do
                crossoverChance <- randomRIO (0, 1.0) :: IO Double
                let chromasomeLength = length c1
                crossPoint <- randomRIO (0, chromasomeLength)
                let (cc1, cc2) = if crossoverChance < 0.7 then crossover crossPoint (c1, c2) else (c1, c2)
                bits1 <- replicateM (quot chromasomeLength 10) $ randomRIO (0, chromasomeLength)
                bits2 <- replicateM (quot chromasomeLength 10) $ randomRIO (0, chromasomeLength)
                let mut1 = doMutation bits1 cc1
                let mut2 = doMutation bits2 cc2
                return $ mut1 : mut2 : []

iteration :: Diagram Cairo R2 -> (Int, [Chromasome]) -> IO (Int, [Chromasome])
iteration baseImage (count, population) = do
                let comparison = imageValue baseImage

                -- create a score for each chromasome
                let images = fmap generateImage population

                scores <- mapM comparison images


                -- find the best score and write it to disk

                let filename = printf "/vagrant/caravaggio/target/out/%010d.png" count

                let imageAndScore = zip images scores
                let (bestImage, score) = head $ sortBy (compare `on` snd) imageAndScore

                putStrLn $ "Iteration " ++ show count ++ " Score: " ++ show score

                persistImage filename 400 400 bestImage


                -- generate new chromasomes


                newChromasomes <- doGenetics $ zip population scores

                -- Go again

                iteration baseImage (count + 1, newChromasomes)

randomChromasome :: IO Chromasome
randomChromasome = replicateM bitCount $ randomRIO (False, True)

run :: Diagram Cairo R2 -> IO ()
run baseImage = do
    firstPop <- replicateM 10 randomChromasome
    iteration baseImage (1, firstPop)
    return ()

main = do
  imageEither <- fromDisk "/vagrant/svg/target/MonaLisa.png"
  case imageEither of
             Left error -> putStrLn $ "ERROR: " ++ error
             Right img -> run img
