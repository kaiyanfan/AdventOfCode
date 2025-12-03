import Data.List.Split (splitOn)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print (solve1 input)
    print (solve2 input)

solve1 s = sum (zipWith (*) (map (\x -> let (a,b) = splitAt (length (show x) `div` 2) (show x) in fromEnum (a == b)) (parse s)) (parse s))

solve2 s = sum (zipWith (*) (map (\x -> let s = show x in fromEnum $ any (\l -> take (length s) (cycle (take l s)) == s && length s `mod` l == 0) [1 .. length s `div` 2]) (parse s)) (parse s))

parse s = concatMap (\r -> let [a,b] = splitOn "-" r in [read a+0..read b]) (splitOn "," s)
