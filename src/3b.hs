import Data.Char

main :: IO ()
main = do
    file_contents <- readFile "input/3.txt"
    let compartments = get_compartments (lines file_contents)
    let overlaps = foldl (\x acc -> acc ++ x) [] (map rm_dup (map get_overlap compartments))
    let priority_sum = sum (map get_priority overlaps)
    print priority_sum

get_compartments :: [String] -> [(String, String)]
get_compartments lines = map (\x -> splitAt ((length x) `div` 2) x) lines

get_overlap :: (String, String) -> String
get_overlap (a, b) = filter (\x -> elem x b) a

get_priority :: Char -> Int
get_priority c = if elem c ['a'..'z']
    then (ord c) - (ord 'a') + 1
    else (ord c) - (ord 'A') + 27

rm_dup :: Eq a => [a] -> [a]
rm_dup [] = []
rm_dup (x:xs)   | elem x xs = rm_dup xs
                | otherwise = x : rm_dup xs
