main = do
    input <- getContents
    print (solve1 input)
    print (solve2 input)

solve1 s = length $ filter (\x -> x `mod` 100 == 0) (gen s)

solve2 s = sum (zipWith (\a b -> (max a b - 1) `div` 100 - min a b `div` 100) arr (drop 1 arr)) + solve1 s where arr = gen s

gen s = scanl (+) 10050 (map (\(d:xs) -> (if d=='R' then 1 else -1) * read xs) (lines s))
