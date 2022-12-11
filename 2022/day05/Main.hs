import AocLib
import Data.List
import Text.Regex.Posix

filt :: [a] -> [a]
filt [] = []
filt (_:[]) = []
filt (v:_:[]) = [v]
filt (_:_:_:[]) = []
filt (v:_:_:_:hs) = v:(filt hs)

rem_spaces :: String -> String
rem_spaces [] = []
rem_spaces (' ':hs) = rem_spaces hs
rem_spaces (v:hs) = v:(rem_spaces hs)

dup :: [Int] -> [[Int]]
dup (0:hs) = []
dup (n:hs) = hs : (dup ((n-1):hs))

mod' :: Int -> Int -> Int -> Int -> [String] -> [String]
mod' n f t i l 
    | f == i = (drop n (l !! i)) : (mod' n f t (i+1) l)
    | t == i = (((take n (l !! f)) ++ (l !! t))) : (mod' n f t (i+1) l)
    | i < (length l) = (l !! i) : (mod' n f t (i+1) l)
    | otherwise = []

move :: Int -> Int -> Int -> [String] -> [String]
move n f t l = mod' n f t 0 l

minus_one :: Int -> Int
minus_one x = x - 1

minus_ones :: [Int] -> [Int]
minus_ones l = map minus_one l

minus_ones_ex :: [Int] -> [Int]
minus_ones_ex (h:l) = h:(minus_ones l)

ints :: [String] -> [Int]
ints = map read

main = do
    dat <- read_input
    let input = split_empty dat
    let cranes = (map rem_spaces  (transpose $ (map (filt . (drop 1)) (head  input))))

    let rules = last input
    print cranes
    print rules

    let reg = "[0-9]+"
    let res = map (\s -> getAllTextMatches ((s =~ reg) :: AllTextMatches [] String)) rules
    let rules = map (minus_ones_ex . ints) res
    print rules
    let res = (foldl (\l v -> move (head v)  (v !! 1) (last v) l) cranes rules)
    print (foldr (\s a -> (head s) : a) [] res)

