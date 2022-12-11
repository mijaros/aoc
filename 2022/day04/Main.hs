import AocLib
import Data.List.Split

to_tuple :: [a] -> (a,a)
to_tuple (u:v:[]) = (u,v)

to_tuples' :: [[a]] -> [(a,a)]
to_tuples' lst = map to_tuple lst

to_tuples :: [(a,a)] -> ((a,a),(a,a))
to_tuples lst = to_tuple lst

tuples :: [[a]] -> ((a,a),(a,a))
tuples lst = to_tuples $ to_tuples' $ lst

check :: ((Int,Int),(Int,Int)) -> Bool
check ((ll,lu),(rl,ru))
    | ll >= rl && ll <= ru = True
    | rl >= ll && rl <= lu = True
    | otherwise = False

addOne :: Bool -> Int
addOne True = 1
addOne False = 0

main :: IO ()
main = do
    dat <- read_input
    let res = map tuples [map (\l -> int_reader (splitOn "-" l)) x | x <- map (splitOn ",") dat]
    print res
    let overlaps = map check res
    print overlaps
    print (foldl (\u v -> u + (addOne v)) 0 overlaps)


int_reader :: [String] -> [Int]
int_reader = map read
