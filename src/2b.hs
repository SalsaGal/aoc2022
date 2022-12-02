main :: IO ()
main = do
    file <- readFile "input/2.txt"
    --let strategy = map move_to_line ["A Y", "B X", "C Z"]
    let strategy = map move_to_line (lines file)
    print strategy
    let moves = map with_required strategy
    print moves
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
                'X' -> 'L'
                'Y' -> 'D'
                'Z' -> 'W'
        )
    ]

-- Takes move, outcome
-- Returns move, my move
with_required :: [Char] -> [Char]
with_required [a@'R', 'L'] = [a, 'S']
with_required [a@'P', 'L'] = [a, 'R']
with_required [a@'S', 'L'] = [a, 'P']
with_required [move, 'D'] = [move, move]
with_required [a@'R', 'W'] = [a, 'P']
with_required [a@'P', 'W'] = [a, 'S']
with_required [a@'S', 'W'] = [a, 'R']

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
