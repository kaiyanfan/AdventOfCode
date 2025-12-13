import Data.List (group, sort, sortOn)
import Data.List.Split (splitOn)
import Data.Ord (Down(..))

main :: IO ()
main = do
    input <- readFile "input.txt"
    print (solve1 input)
    print (solve2 input)

solve1 s = product . take 3 . sortOn Down . map length . group . sort . foldl step [0..length (lines s) - 1] . take 1000 $ parse s

solve2 s = b where (_, b, _, _) = parse s !! (length (takeWhile ((> 1) . length . group) $ scanl step [0 .. length (lines s) - 1] $ parse s) - 1)

dist p1 p2 = sum $ zipWith (\a b -> (a - b) ^ 2) p1 p2

step l (_, _, a, b) = map (\v -> if v == (l !! b) then l !! a else v) l

parse s = sortOn (\(x, _, _, _) -> x) [(dist p1 p2, head p1 * head p2, i1, i2) | (p1, i1) <- spc, (p2, i2) <- spc, i1 < i2] where spc = zip (map (map read . splitOn ",") $ lines s) [0..]