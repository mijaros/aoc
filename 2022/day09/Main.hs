import AocLib
import Data.List.Split
import Data.List

data Move = MUp Int | MDown Int | MLeft Int | MRight Int | MError String deriving (Show)

parse_move :: [String] -> Move
parse_move ("U":x:[]) = MUp (read x)
parse_move ("D":x:[]) = MDown (read x)
parse_move ("L":x:[]) = MLeft (read x)
parse_move ("R":x:[]) = MRight (read x)
parse_move _ = MError "Failed parsing"

process_head :: [(Int,Int)] -> [Move] -> [(Int,Int)]
process_head l [] = l
process_head ((x,y):hs) (curr:ms) = process_head ((reverse $ n $ curr) ++ ((x,y):hs)) ms
    where n (MUp v) = [(x+k,y) | k <- [1..v]]
          n (MDown v) = [(x-k,y) | k <- [1..v]]
          n (MLeft v) = [(x,y-k) | k <- [1..v]]
          n (MRight v) = [(x,y+k) | k <- [1..v]]
                            
get_next :: (Int,Int) -> (Int,Int) -> (Int,Int)
get_next (x,y) (u,v)
    | x == u && y == v                              = (x,y)
    | (abs (x-u)) <= 1 && (abs (y-v)) <= 1          = (x,y)
    | (abs (x-u)) < (abs (y-v)) && (y-v > 0)        = (u,v+1)
    | (abs (x-u)) < (abs (y-v)) && (v-y > 0)        = (u,v-1)
    | (abs (y-v)) < (abs (x-u)) && (x - u > 0)      = (u+1,v)
    | (abs (y-v)) < (abs (x-u)) && (u - x > 0)      = (u-1,v)
    | (abs (y-v)) == (abs(x-u)) && (x - u > 0) && (y-v > 0)    = (u+1,v+1)
    | (abs (y-v)) == (abs(x-u)) && (x - u > 0) && (v-y > 0)    = (u+1,v-1)
    | (abs (y-v)) == (abs(x-u)) && (u - x > 0) && (y-v > 0)    = (u-1,v+1)
    | (abs (y-v)) == (abs(x-u)) && (u - x > 0) && (v-y > 0)    = (u-1,v-1)
    | otherwise                                     = (333,333)

process_tail :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
process_tail l [] = l
process_tail (t:ts) (h:hs) = process_tail ((get_next t h):t:ts) hs

process_tails :: [[(Int,Int)]] -> Int -> [[(Int,Int)]]
process_tails l 0 = l
process_tails (p:ps) n = process_tails ((reverse $ process_tail [(0,0)] p ):p:ps) (n-1)


main :: IO ()
main = do
    input <- read_input
    let parsed = (map (\v -> parse_move $ splitOn " " $ v) input)
    print parsed
    let head_moves = reverse $ process_head [(0,0)] parsed
    print head_moves
    let tail_moves = process_tails [head_moves] 9
    let uniq_places =  uniq $ sort (head tail_moves)
    print $ length uniq_places

