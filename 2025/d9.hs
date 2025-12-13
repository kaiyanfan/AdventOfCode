import Data.List.Split (splitOn)
import Data.Tuple (swap)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print (solve1 input)
    print (solve2 input)

solve1 s = maximum [area p1 p2 | p1 <- ps, p2 <- ps] where ps = parse s

solve2 s = maximum [area p1 p2 | p1 <- ps, p2 <- ps, not . intersect p1 p2 $ zip ps (tail ps ++ [head ps])] where ps = parse s

area (x1,y1) (x2,y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

intersect (x1,y1) (x2,y2) = any $ \((x3,y3),(x4,y4)) -> not $ min x1 x2 >= max x3 x4 || max x1 x2 <= min x3 x4 || min y1 y2 >= max y3 y4 || max y1 y2 <= min y3 y4

parse s = map (\l -> let (a, _ : b) = break (== ',') l in (read a, read b) ) $ lines s
