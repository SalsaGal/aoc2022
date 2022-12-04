main :: IO ()
main = do
    input <- readFile "input/4.txt"
    let pairs = map (\x -> split x ',') (lines input)
    print pairs
    let ranges = map (\(x,y) -> (str_to_range x, str_to_range y)) pairs
    print ranges
    let full_overlaps = filter (\(x,y) -> contains x y || contains y x) ranges
    print full_overlaps
    print (length full_overlaps)

contains :: [Int] -> [Int] -> Bool
contains a b = do
    let unique = filter (\x -> elem x a) b
    length unique /= 0

split :: String -> Char -> (String, String)
split str delim = do
    let comma_pos = snd ((filter (\x -> fst x == delim) (zip str [0..])) !! 0)
    let (a,b) = splitAt comma_pos str
    (a, tail b)

str_to_range :: String -> [Int]
str_to_range str = do
    let (a,b) = split str '-'
    [(read a)..(read b)]
