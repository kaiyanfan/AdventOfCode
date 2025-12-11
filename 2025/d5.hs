import Data.List.Split (splitOn)
import Data.List (sort)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print (solve1 input)
    print (solve2 input)

solve1 s = sum $ map (\x -> fromEnum (any (inside x) rs)) xs where (rs, xs) = parse s

solve2 s = fst $ foldl acc (0, 0) rs where rs = sort (fst (parse s))

parse s = (map (map read . splitOn "-") rs, map ((+0) . read) xs) where [rs, xs] = map lines (splitOn "\n\n" s)

acc (sum, x) [l, r] = (sum + max 0 (r - max (x + 1) l + 1), max x r)

inside x [l, r] = l <= x + 0 && x <= r

