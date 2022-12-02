main :: IO ()
main = do
    input <- readFile "input/1.txt"
    let input_split = lines input
    let sums = foldl handle_input [0] input_split
    print (maximum sums)

handle_input :: [Int] -> String -> [Int]
handle_input acc line =
    if line == "" then acc ++ [0]
    else (init acc) ++ [(last acc) + read line]
