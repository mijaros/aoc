import Data.List
import AocLib

to_i :: String -> Int
to_i = read

merge :: ((Int,Bool),(Int,Bool)) -> (Int, Bool)
merge ((a,b),(c,d)) = (a,(b||d))

overlay :: [[(Int,Bool)]] -> [[(Int,Bool)]] -> [[(Int,Bool)]]
overlay a b = [map merge x | x <- (map (\s -> zip (fst s) (snd s)) (zip a b))]

filt :: [Int] -> Int -> [Int]
filt lis x = if x > (maximum lis)
                then x:lis
                else lis

solve_one :: [Int] -> [Int]
solve_one x = reverse $ foldl (\a v -> filt a v) [head x] (tail x)

combine :: [Int] -> [Int] -> [(Int,Bool)]
combine _ [] = []
combine [] (h:hs) = (h,False) : (combine [] hs)
combine (p:ps) (h:hs)
    | p == h = (p,True) : (combine ps hs)
    | otherwise = (h,False) : (combine (p:ps) hs)

combine_tuple :: ([Int],[Int]) -> [(Int,Bool)]
combine_tuple (l,r) = combine l r

solver :: [[Int]] -> [[(Int,Bool)]]
solver lis = map combine_tuple (zip (map solve_one lis) lis)

counter :: [[(Int,Bool)]] -> Int
counter lis = foldl (+) 0 [foldl (\a x -> a + if (snd x) then 1 else 0) 0 v | v <- lis]

my_filt :: Int -> [Int] -> [Int]
my_filt _ [] = []
my_filt v (h:hs)
    | v > h = h:(my_filt v hs)
    | otherwise = [h]

tuple_filt :: (Int, [Int], [Int]) -> (Int,[Int],[Int])
tuple_filt (v,right,left) = (v,(my_filt v right),(my_filt v left))

my_split :: [Int] -> [Int] -> [(Int,[Int],[Int])]
my_split pre [] = []
my_split pre (i:res) = (i,(reverse pre),res) : (my_split (pre++[i]) res)

my_merge_2 :: (Int,[Int],[Int]) -> (Int,[Int],[Int]) -> (Int)
my_merge_2 (v,l,r) (v2,u,d) = (length l)*(length r)*(length u)*(length d)

my_merge_1 :: [(Int,[Int],[Int])] -> [(Int,[Int],[Int])] -> [Int]
my_merge_1 [] _ = []
my_merge_1 _ [] = []
my_merge_1 (l:ls) (r:rs) = (my_merge_2 l r) : (my_merge_1 ls rs)

my_merge :: [[(Int,[Int],[Int])]] -> [[(Int,[Int],[Int])]] -> [[Int]]
my_merge [] _ = []
my_merge _ [] = []
my_merge (l:ls) (r:rs) = (my_merge_1 l r) : (my_merge ls rs)

main :: IO ()
main = do
    inp <- read_input 
    let mat = [map (\c -> to_i [c]) x | x <- inp]
    print mat
    let horizontal = (map (my_split []) mat)
    let vertical =  (transpose (map (my_split []) (transpose mat)))
    let f_horizontal = [map tuple_filt x | x <- horizontal]
    let f_vertical = [map tuple_filt x | x <- vertical]
    let result = (my_merge f_horizontal f_vertical)
    print result
    print (maximum $ map maximum result)
