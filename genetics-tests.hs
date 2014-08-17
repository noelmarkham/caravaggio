import Test.QuickCheck
import Text.Printf
import Genetics
import Data.List (nub)

----------- doMutation

prop_doMutation_length bits chromasome = length (doMutation bits chromasome) == length chromasome

prop_doMutationDoMutation bits chromasome = (mutationForBits . mutationForBits) chromasome == chromasome
    where mutationForBits = doMutation bits

prop_doMutation_fullFlip chromasome = doMutation indices chromasome == negated
    where indices = [1..(length chromasome)]
          negated = fmap not chromasome

prop_doMutation_noBits chromasome = doMutation [] chromasome == chromasome

----------- crossover

prop_crossover_length pair @ (c1, c2) index = length crossed1 + length crossed2 == length c1 + length c2
    where (crossed1, crossed2) = crossover index pair

prop_crossoverCrossover elements index = (crossedover . crossedover) pair == pair
    where pair = (fmap fst elements, fmap snd elements)
          crossedover = crossover index

----------- roulette choice

-- ([(a, Int)] -> [(a, Int)]) -> Int -> [(a, Int)] -> (a, Int)

prop_createRouletteProportions_order population = createdProportions == original
    where original = fmap fst population
          createdProportions = fmap fst $ createRouletteProportions population

prop_createRouletteProportions_scoreIncreases prelimPopulation = all (\(a, b) -> a <= b) slidingScores
    where population = fmap (\(a, b) -> (a, abs b)) prelimPopulation
          scores = fmap snd $ createRouletteProportions population
          slidingScores = zip scores $ tail scores

-----------

tests = [
         ("doMutation keeps chromasomes the same length",                quickCheck prop_doMutation_length),
         ("doMutation.doMutation returns the original chromasome",       quickCheck prop_doMutationDoMutation),
         ("doMutation with every index negates the original chromasome", quickCheck prop_doMutation_fullFlip),
         ("doMutation with no indices returns the original chromasome",  quickCheck prop_doMutation_noBits),

         ("crossover keeps chromasomes the same length",  quickCheck prop_crossover_length),
         ("crossover.crossover reurns the original pair", quickCheck prop_crossoverCrossover),

         ("createRouletteProportions preserves the order",                                        quickCheck prop_createRouletteProportions_order),
         ("createRouletteProportions score only ever increases",                                  quickCheck prop_createRouletteProportions_scoreIncreases)
        ]

main = mapM_ (\(s, a) -> printf "%-40s: " s >> a) tests
