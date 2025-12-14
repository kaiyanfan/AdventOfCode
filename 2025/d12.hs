import Data.List.Split (splitOn)
import GHC.Float (int2Float)

main = do
    input <- getContents
    print (solve1 input)

solve1 s = length . filter id $ map (\(area, ns) -> area >= sum (zipWith (*) tiles ns)) regs where (tiles, regs) = parse s

parse s = (map (length . filter (== '#')) (init ls), map parseLine $ lines $ last ls) where ls = splitOn "\n\n" s

parseLine l = (\(a:b:_) -> (a*b, map read . tail $ words l)) . map read . splitOn "x" . init . head $ words l