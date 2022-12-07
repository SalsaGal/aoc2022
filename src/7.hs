main :: IO ()
main = do
    input <- readFile "input/7example.txt"
    let instructions = parse_instructions input
    let (file, pwd) = foldl get_file ([], []) instructions :: ([File], [String])
    print file
    print pwd

get_file :: ([File], [String]) -> Instruction -> ([File], [String])
get_file (files, pwd) instruction = case head (split ' ' (command instruction)) of
    "cd" -> do
        let target = (split ' ' (command instruction)) !! 1
        case target of
            ".." -> (files, init pwd)
            _ -> (files, pwd ++ [target])
    "ls" -> ([], pwd)

parse_instructions :: String -> [Instruction]
parse_instructions input = foldl (\instructions line ->
        if head line == '$'
        then instructions ++ [Instruction {command=(drop 2 line), outputs=[]}]
        else do
            let end = last instructions :: Instruction
            (init instructions) ++ [Instruction {
                command=command end,
                outputs=(outputs end) ++ [line]
            }]
    ) [] (lines input)

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

data Instruction = Instruction {
    command :: String,
    outputs :: [String]
} deriving Show

data File =
    Folder { name :: String, files :: [File]} |
    File { name :: String, size :: Int }
    deriving Show
