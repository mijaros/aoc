import AocLib
import Data.List
import Data.List.Split
import Data.Char

halves :: [a] -> ([a],[a])
halves lst = splitAt ((length lst) `div` 2) lst


common :: (Eq a) => ([a],[a]) -> [a]
common (l,r) = intersect l r

normalize :: Char -> Int
normalize x
    | x >= 'a' && x <= 'z' = (ord x) - (ord 'a') + 1
    | x >= 'A' && x <= 'Z' = (ord x) - (ord 'A') + 27
    | otherwise = 0

to_tripletes :: [a] -> [[a]]
to_tripletes [] = []
to_tripletes lst = (take 3 lst) : (to_tripletes (drop 3 lst))

inter :: (Eq a) => [[a]] -> [a]
inter (a:b:c:[]) = intersect a (intersect b c)

main = do
    dat <- read_input
    let res =  (map (uniq . inter) (to_tripletes  dat))
    let flatten = map normalize (foldl (\c v -> c ++ v) [] res)
    print flatten
    print $ sum $ flatten
