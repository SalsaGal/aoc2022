main :: IO ()
main = do
    input <- readFile "input/5example.txt"
    let (stack_start, moves) = split (lines input) ""
    print (parse_stacks stack_start)

split :: Eq a => [a] -> a -> ([a], [a])
split list delim = do
    let pos = fst (filter (\x -> snd x == delim) (zip [0..] list) !! 0)
    let (a,b) = splitAt pos list
    (a, tail b)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks num input = [take num input] ++ chunks num (drop num input)

parse_stacks :: [String] -> [String]
parse_stacks rows = do
    let lines = reverse (init (map (\x -> map (\y -> y !! 1) (chunks 4 x)) rows)) :: [String]
    init (
            map (\x -> filter (\char -> char /= ' ') x) (
                map (\index ->
                    map (\x ->
                        if index < length x
                            then x !! index
                            else ' '
                    ) lines
                ) [0..length lines]
            )
        )
