import Test.QuickCheck
import Text.Printf
import Genetics

----------- doMutation

prop_doMutation_length bits chromasome = length (doMutation bits chromasome) == length chromasome

prop_doMutationDoMutation bits chromasome = (mutationForBits . mutationForBits) chromasome == chromasome
    where mutationForBits = doMutation bits

----------- crossover

prop_crossover_length pair @ (c1, c2) index = length crossed1 + length crossed2 == length c1 + length c2
    where (crossed1, crossed2) = crossover index pair

prop_crossoverCrossover elements index = (crossedover . crossedover) pair == pair
    where pair = (fmap fst elements, fmap snd elements)
          crossedover = crossover index

-----------

tests = [
         ("doMutation keeps chromasomes the same length",          quickCheck prop_doMutation_length),
         ("doMutation.doMutation returns the original chromasome", quickCheck prop_doMutationDoMutation),
         ("crossover keeps chromasomes the same length",           quickCheck prop_crossover_length),
         ("crossover.crossover reurns the original pair",          quickCheck prop_crossoverCrossover)
        ]

main = mapM_ (\(s, a) -> printf "%-40s: " s >> a) tests
