import AocLib
import Data.List.Split
import Text.Printf

data Inst = Add Int | Nop | Error String deriving (Show)

parse_inst :: [String] -> Inst
parse_inst ("noop":[]) = Nop
parse_inst ("addx":v:[]) = Add (read v)
parse_inst er = Error ("Invalid input " ++ (show er))

run_inst :: [Inst] -> [Int] -> [Int]
run_inst [] l = l
run_inst ((Add i):is) (l:hs) = run_inst is ((l+i):l:l:hs)
run_inst (Nop:is) (l:hs) = run_inst is (l:l:hs)
run_inst ((Error _):_) _ = []

pix_ind :: Int -> Int -> Char
pix_ind n v
    | (abs (v - n)) <= 1 = '#'
    | otherwise = '.'

draw_line :: [Int] -> Int -> String
draw_line [] _ = ""
draw_line (h:hs) i = (pix_ind h i):(draw_line hs (i + 1))

draw :: [Int] -> [String]
draw [] = []
draw v = (draw_line (take 40 v) 0):(draw (drop 40 v))

canvas :: [String] -> String
canvas [] = ""
canvas (v:vs) = (printf "%s\n" v)  ++ (canvas vs)

main :: IO ()
main = do
    input <- read_input
    let parsed = (map (\v -> parse_inst $ splitOn " " $ v) input)
    let inst = init $ reverse $ run_inst parsed [1]
    let divider = map (\_ -> '=' ) [1..40]
    
    putStrLn ("Part One:\n" ++ divider)
    let insts = (take 6 [ ((x*40) + 20) | x <- [0..]])
    let res = [x * (inst !! (x - 1)) | x <- insts]
    print $ sum res
    
    putStrLn (divider ++ "\n\n" ++ "Part 2")
    let mat = draw inst
    putStrLn $ canvas mat

