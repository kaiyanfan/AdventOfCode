import qualified Data.Map as M

main = do
    input <- getContents
    print (solve1 input)
    print (solve2 input)

solve1 s = path "you" "out" $ parse s

solve2 s = path "svr" "fft" m * path "fft" "dac" m * path "dac" "out" m + path "svr" "dac" m * path "dac" "fft" m * path "fft" "out" m where m = parse s

path src dst m = memo M.! src where memo = M.fromList [(n, go n) | n <- M.keys m ++ [dst, "out"]]; go n | n == dst = 1 | n == "out" = 0 | otherwise = sum [memo M.! x | x <- m M.! n]

parse s = M.fromList . map (\s -> let (k,_:v) = break (==':') s in (k, words v)) $ lines s