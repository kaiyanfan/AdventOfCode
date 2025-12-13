import Data.List (elemIndex, elemIndices)
import Data.Maybe (fromJust)
import qualified Data.Map as M

main :: IO ()
main = do
    input <- readFile "input.txt"
    print (solve1 input)
    print (solve2 input)

solve1 = snd . foldl step l g where (l, g) = parse s

solve2 = sum . map snd . fst . foldl step l g where (l, g) = parse s

solve' s = foldl step l g where (l, g) = parse s

step (l1, cnt) l2 = (M.toList $ M.fromListWith (+) $ filter (\(x,_) -> notElem x l2) l1 ++ [x | (i, j) <- s, x <- [(i-1,j), (i+1,j)]], cnt + length s) where s = filter (\(x,_) -> x `elem` l2) l1

parse s = (([(fromJust $ elemIndex 'S' $ head ls, 1)], 0), map (elemIndices '^') $ tail ls) where ls = lines s