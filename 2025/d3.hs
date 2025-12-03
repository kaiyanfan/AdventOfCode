import Data.Char (digitToInt)
import Data.List (maximumBy)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print (solve1 input)    
    print (solve2 input)

solve1 s = sum (map (solve' 2) (lines s))

solve2 s = sum (map (solve' 12) (lines s))

solve' 1 s = fst (mi s)
solve' d s = d1 * (10 ^ (d - 1)) + solve' (d - 1) (drop i s) where (d1, i) = mi (take (length s - d + 1) s)

mi s = maximumBy (\(x1, i1) (x2, i2) -> compare x1 x2 <> compare i2 i1) (zip (map digitToInt s) [1..])
