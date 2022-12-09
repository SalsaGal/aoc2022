module Main where

main :: IO ()
main = do
    input <- readFile "input/8.txt"
    let tiles = map (\line -> map (\x -> read [x]) line) (lines input) :: Map
    let visibilities = map (
            \y -> map (\x -> visible tiles (x, y)) [0..length (head tiles) - 1]) [0..length tiles - 1]
    let visibilities_flat = foldl (\acc list -> acc ++ list) [] visibilities
    print (length (filter (True ==) visibilities_flat))

visible :: Map -> (Int, Int) -> Bool
visible tiles pos@(x, y) = do
    let height = tiles !! y !! x
    any (True ==) (map (\direction -> all (height >) (map_items tiles pos direction)) all_dirs)

-- Returns items to the [Direction] of the position
map_items :: Map -> (Int, Int) -> Direction -> [Int]
map_items tiles (x, y) Main.Left = map (\i -> tiles !! y !! i) [0..x - 1]
map_items tiles (x, y) Main.Right = map (\i -> tiles !! y !! i) [x + 1..length (tiles !! y) - 1]
map_items tiles (x, y) Main.Up = map (\i -> tiles !! i !! x) [0..y - 1]
map_items tiles (x, y) Main.Down = map (\i -> tiles !! i !! x) [y + 1..length tiles - 1]

type Map = [[Int]]

data Direction = Up | Down | Left | Right

all_dirs = [Main.Up, Main.Down, Main.Left, Main.Right]
