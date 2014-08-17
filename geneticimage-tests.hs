import Data.Colour
import Data.Colour.SRGB
import Test.QuickCheck
import Text.Printf
import GeneticImage

instance (Ord a, Floating a, Arbitrary a) => Arbitrary (AlphaColour a) where
  arbitrary = do
         r <- arbitrary
         g <- arbitrary
         b <- arbitrary
         alpha <- arbitrary
         return $ (sRGB24 r g b) `withOpacity` alpha
  shrink = shrinkNothing

----------- diffPix

prop_diffPix_same c = diffPix c c == 0
prop_diffPix_size c d = abs (diffPix c d) == abs (diffPix d c)

-----------

tests = [
         ("Difference score for the same alphacolour is 0", quickCheck prop_diffPix_same),
         ("Size of the difference is the same regardless of param order", quickCheck prop_diffPix_size)
        ]

main = mapM_ (\(s, a) -> printf "%-40s: " s >> a) tests
