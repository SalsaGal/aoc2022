main :: IO ()
main = do
    strategy <- readFile "input/2.txt"
    let moves = map move_to_line (lines strategy)
    let scores = map score moves
    print scores
    print (sum scores)

move_to_line :: String -> [Char]
move_to_line move = [
        (
            case move !! 0 of
                'A' -> 'R'
                'B' -> 'P'
                'C' -> 'S'
        ),
        (
            case move !! 2 of
                'X' -> 'R'
                'Y' -> 'P'
                'Z' -> 'S'
        )
    ]

score :: [Char] -> Int
score moves = 1 + move_index (last moves) + round_score (map move_index moves)

round_score :: [Int] -> Int
round_score [a, b] =
    if ((a + 1) `mod` 3) == b then 6
    else if a == b then 3
    else 0

move_index :: Char -> Int
move_index 'R' = 0
move_index 'P' = 1
move_index 'S' = 2
