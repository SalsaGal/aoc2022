module Main where

main :: IO ()
main = do
    input <- readFile "input/8example.txt"
    let tiles = map (\line -> map (\x -> read [x]) line) (lines input) :: Map
    print (map_items tiles (1,1) Main.Left)
    print (map_items tiles (1,1) Main.Right)
    print (map_items tiles (1,1) Main.Up)
    print (map_items tiles (1,1) Main.Down)

invisible :: Map -> (Int, Int) -> Bool
invisible tiles pos@(x, y) = do
    let height = tiles !! y !! x :: Int
    any (\lines -> any (height <=) lines) (map (map_items tiles pos) all_dirs)

-- Returns items to the [Direction] of the position
map_items :: Map -> (Int, Int) -> Direction -> [Int]
map_items tiles (x, y) Main.Left = map (\i -> tiles !! y !! i) [0..x - 1]
map_items tiles (x, y) Main.Right = map (\i -> tiles !! y !! i) [x + 1..length (tiles !! y) - 1]
map_items tiles (x, y) Main.Up = map (\i -> tiles !! i !! x) [0..y - 1]
map_items tiles (x, y) Main.Down = map (\i -> tiles !! i !! x) [y + 1..length tiles - 1]

type Map = [[Int]]

data Direction = Up | Down | Left | Right

all_dirs = [Main.Up, Main.Down, Main.Left, Main.Right]
