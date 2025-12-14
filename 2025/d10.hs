import Data.List (elemIndices, group, sort, subsequences)
import Data.List.Split (splitOn)

main = do
    input <- getContents
    print (solve1 input)
    print (solve2 input)

solve1 s = sum . map step $ parse s where step (tgt, xs, _) = minimum [length sub | sub <- subsequences xs, (map head . filter (odd . length) . group . sort . concat) sub == tgt]

solve2 s = sum . map (\(_, xs, tgt) -> step xs tgt) $ parse s

step xs tgt | all (== 0) tgt = 0 | null rst = 9999 | otherwise = minimum rst where rst = map (\(lsub, tgt') -> lsub + step xs (map (`div` 2) tgt') * 2) $ step' xs tgt

step' xs tgt = [(length sub, tgt') | sub <- subsequences xs, let tgt' = zipWith (-) tgt [length $ concatMap (filter (== i)) sub | i <- [0..length tgt - 1]], all (\x -> even x && x >= 0) tgt']

parse s = map (\l -> let ls = words l in (map (subtract 1) . elemIndices '#' $ head ls, map p2 $ mid ls, p2 $ last ls)) $ lines s where mid = init . tail; p2 = map read . splitOn "," . mid
