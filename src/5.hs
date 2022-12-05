main :: IO ()
main = do
    input <- readFile "input/5example.txt"
    let [stack_start, moves] = take 2 (split "" (lines input))
    print (parse_stacks stack_start)
    print (split ',' "foo,bar,baz")

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split delim list = do
    let delims = (filter (\x -> snd x == delim) (zip [0..] list))
    if null delims
        then [list]
        else do
            let pos = fst (head delims)
            let (a,b) = splitAt pos list
            [a] ++ split delim (tail b)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks num input = [take num input] ++ chunks num (drop num input)

parse_stacks :: [String] -> [String]
parse_stacks rows = do
    let lines = (init (map (\x -> map (\y -> y !! 1) (chunks 4 x)) rows)) :: [String]
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
