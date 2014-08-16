import Test.QuickCheck
import Text.Printf

prop_reversereverse s = (reverse . reverse) s == id s
    where _ = s :: [Int]

tests = [("reverse.reverse/id", quickCheck prop_reversereverse)]

main = mapM_ (\(s, a) -> printf "%-25s: " s >> a) tests
