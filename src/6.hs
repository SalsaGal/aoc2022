main :: IO ()
main = do
    input <- readFile "input/6.txt"
    let transmissions = lines input
    print (map get_start transmissions)

get_start :: String -> Int
get_start message = fst (head (filter (\(_,x) -> unique x) (zip [0..] (window 4 message)))) + 4

unique :: String -> Bool
unique input = not (any (1<) (map (\x -> length (filter (x==) input)) input))

window :: Int -> String -> [String]
window _ "" = []
window num list = [take num list] ++ window num (tail list)
