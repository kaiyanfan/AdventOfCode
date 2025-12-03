import Data.List (transpose)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print (solve1 input)
    print (solve2 input)

solve1 s = count (`elem` [1..4]) (zipWith (zipWith (*)) (map adj (transpose (map adj m))) (transpose m)) where m = parse s

solve2 s = count (>0) m - count (>0) (iterate (\m -> map (map (fromEnum . (>=5))) (zipWith (zipWith (*)) (transpose(map adj (transpose (map adj m)))) m)) m !! 100) where m = parse s

parse s = map (map (\c -> if c == '@' then 1 else 0)) (lines s)

adj l = zipWith3 (\a b c -> a + b + c) l (tail l ++ [0]) (0 : init l)

count f m = length $ concatMap (filter f) m
