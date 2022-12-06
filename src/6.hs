main :: IO ()
main = do
    input <- readFile "input/6example.txt"
    let transmissions = lines input
    print (unique "foab")
    print (unique "boab")

unique :: String -> Bool
unique input = not (any (1<) (map (\x -> length (filter (x==) input)) input))

window :: Int -> String -> [String]
window _ "" = []
window num list = [take num list] ++ window num (tail list)
