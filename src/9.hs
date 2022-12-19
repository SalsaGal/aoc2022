main :: IO ()
main = do
    instructions <- readFile "input/9example.txt"
    print (foldl do_move default_map (lines instructions))

data Map = Map {
    h :: (Int, Int),
    t :: (Int, Int)
} deriving Show


do_move :: Map -> String -> Map
do_move tiles instruction = do
    let tokens = split ' ' instruction
    let new_h = mul_pos (dir_pos (head (tokens !! 0))) (read (tokens !! 1))
    do_tail_move Map {
        h=add_pos new_h (h tiles),
        t=t tiles
    }

do_tail_move :: Map -> Map
do_tail_move tiles = do
    let difference = sub_pos (h tiles) (t tiles)
    let x_diff = if fst difference > 1 then 1
        else if fst difference < -1 then -1
        else 0
    let y_diff = if snd difference > 1 then 1
        else if snd difference < -1 then -1
        else 0

    Map {
        h=h tiles,
        t=add_pos (x_diff, y_diff) (t tiles)
    }

add_pos :: (Int, Int) -> (Int, Int) -> (Int, Int)
add_pos (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

sub_pos :: (Int, Int) -> (Int, Int) -> (Int, Int)
sub_pos (a1, a2) (b1, b2) = (a1 - b1, a2 - b2)

mul_pos :: (Int, Int) -> Int -> (Int, Int)
mul_pos (a1, a2) b = (a1 * b, a2 * b)

dir_pos :: Char -> (Int, Int)
dir_pos char = case char of
    'R' -> (1, 0)
    'L' -> (-1, 0)
    'U' -> (0, 1)
    'D' -> (0, -1)

default_map :: Map
default_map = Map { h=(0, 0), t=(0, 0) }

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
