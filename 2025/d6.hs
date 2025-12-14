import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.List (transpose)

main = do
    input <- getContents
    print (solve1 input)
    print (solve2 input)

solve1 s = sum $ map go $ transpose $ map words $ lines s where go xs = op (last xs) $ map read (init xs)

solve2 s = sum $ zipWith go (words $ last $ lines s) $ splitOn [""] $ map (filter (not . isSpace)) $ transpose $ init $ lines s where go c xs = op c $ map read xs

op "*" = product
op "+" = sum