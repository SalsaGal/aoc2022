main :: IO ()
main = do
    input <- readFile "input/7.txt"
    let instructions = parse_instructions input
    let (fs, _) = foldl get_file ([], []) instructions :: ([Item], [String])
    print (sum (filter (100000>) (map get_size (search_fs is_folder fs))))

search_fs :: (Item -> Bool) -> [Item] -> [Item]
search_fs pred fs = foldl (\result file -> do
        let children_adds = if is_folder file
            then search_fs pred (files file)
            else []
        let inherent = if pred file
            then result ++ [file]
            else result
        children_adds ++ inherent
    ) [] fs

get_size :: Item -> Int
get_size item = case item of
    File {name=_, size=_} -> size item
    Folder {name=_, files=_} -> sum (map get_size (files item))

find_add :: [Item] -> [String] -> Item -> [Item]
find_add fs path to_add = do
    let is_correct_name x = head path == name x
    if null path
        then fs ++ [to_add]
        else do
            map (\file -> if head path == name file
                    then Folder {name=name file, files=find_add (files file) (tail path) to_add}
                    else file
                ) fs

get_file :: ([Item], [String]) -> Instruction -> ([Item], [String])
get_file (files, pwd) instruction = case head (split ' ' (command instruction)) of
    "cd" -> do
        let target = (split ' ' (command instruction)) !! 1
        case target of
            ".." -> (files, init pwd)
            "/" -> (files, [])
            _ -> (files, pwd ++ [target])
    "ls" -> do
        let new_files = map ls_to_file (outputs instruction)
        (foldl (\fs new -> find_add fs pwd new) files new_files, pwd)

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
