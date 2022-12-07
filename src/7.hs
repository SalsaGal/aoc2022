main :: IO ()
main = do
    input <- readFile "input/7example.txt"
    let instructions = parse_instructions input
    let (file, pwd) = foldl get_file ([], []) instructions :: ([Item], [String])
    print file

find_add :: Item -> [Item] -> [String] -> [Item]
find_add to_add fs path = do
    let is_correct_name x = head path == name x
    if length fs == 1
        then do
            map (\file -> if is_correct_name file
                    then Folder {name=name file, files=files file ++ [to_add]}
                    else file
                ) fs
        else do
            map (\file -> if is_correct_name file
                    then Folder {name=name file, files=find_add to_add (files file) (tail path)}
                    else file
                ) fs

get_file :: ([Item], [String]) -> Instruction -> ([Item], [String])
get_file (files, pwd) instruction = case head (split ' ' (command instruction)) of
    "cd" -> do
        let target = (split ' ' (command instruction)) !! 1
        case target of
            ".." -> (files, init pwd)
            _ -> (files, pwd ++ [target])
    "ls" -> do
        let new_files = outputs instruction
        (files, pwd)

ls_to_file :: String -> Item
ls_to_file line = do
    let tokens = split ' ' line
    case head tokens of
        "dir" -> Folder {name=tokens !! 1, files=[]}
        _ -> File {name=tokens !! 1, size=read (head tokens)}

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

data Item =
    Folder { name :: String, files :: [Item]} |
    File { name :: String, size :: Int }
    deriving Show

is_file :: Item -> Bool
is_file File { name=_, size=_} = True
is_file Folder { name=_, files=_} = False

is_folder :: Item -> Bool
is_folder File { name=_, size=_} = False
is_folder Folder { name=_, files=_} = True
