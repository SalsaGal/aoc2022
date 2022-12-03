main :: IO ()
main = do
    file_contents <- readFile "input/3example.txt"
    let compartments = get_compartments (lines file_contents)
    let overlaps = map get_overlap compartments
    print overlaps

get_compartments :: [String] -> [(String, String)]
get_compartments lines = map (\x -> splitAt ((length x) `div` 2) x) lines

get_overlap :: (String, String) -> [Char]
get_overlap (a, b) = filter (\x -> elem x b) a
