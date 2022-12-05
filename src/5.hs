main :: IO ()
main = do
    input <- readFile "input/5example.txt"
    let [stack_start, moves] = take 2 (split "" (lines input))

    let stacks = parse_stacks stack_start
    print stacks
    print (do_move (head moves) stacks)

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

do_move :: String -> [String] -> [String]
do_move move list = do
    let [_, count, _, src, _, dest] = split ' ' move
    let moved = reverse (take (read count) (list !! read dest))
    map (\(index, pile) ->
            if index == (read src) - 1 then drop (read count) pile
            else if index == (read dest) - 1 then moved ++ pile
            else pile
        ) (zip [0..] list)

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
