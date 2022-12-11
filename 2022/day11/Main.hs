import AocLib
import Parser
import Data.List

oAppl :: Operation -> Int -> Int
oAppl (Plus k) i = k + i
oAppl (Times k) i = k * i
oAppl Square i = i*i

append_monkey :: Monkey -> [Int] -> Monkey
append_monkey m i = let old = (items m) 
                    in m { items = old ++ i }

clean_monkey :: Monkey -> Monkey
clean_monkey m = m { items = [] }

proc_monkey :: Int -> Monkey -> [Int]
proc_monkey divisor m = map (\v -> (oAppl (ops m) v) `rem` divisor ) (items m)

proc_turn :: Int -> (Monkey,Monkey) -> Monkey -> (Monkey,Monkey,Monkey,(Int,Int))
proc_turn divisor (f,t) m = let res = proc_monkey divisor m
                            in ((clean_monkey m), 
                                (append_monkey f (filter (\v -> v `rem` (test m) /= 0) res)),
                                (append_monkey t (filter (\v -> v `rem` (test m) == 0) res)),
                                ((ident m),(length res)))

reconstruct :: [Monkey] -> (Monkey,Monkey,Monkey) -> [Monkey]
reconstruct [] _ = []
reconstruct (h:mons) (m,f,t) 
    | (ident h) ==  (ident m) = m:(reconstruct mons (m,f,t))
    | (ident h) == (ident f)  = f:(reconstruct mons (m,f,t))
    | (ident h) == (ident t)  = t:(reconstruct mons (m,f,t))
    | otherwise               = h:(reconstruct mons (m,f,t))


turn :: Int -> ([Monkey],[(Int,Int)]) -> Int -> ([Monkey],[(Int,Int)])
turn divisor (m,p) i = let curr = m !! i
                           rest = ((m !! (ifFalse curr)), (m !! (ifTrue curr)))
                           (new,fal,tru,proc) = proc_turn divisor rest curr
                        in (reconstruct m (new,fal,tru),(p ++ [proc]))

round' :: Int -> ([Monkey],[(Int,Int)]) -> Int -> ([Monkey],[(Int,Int)])
round' divisor (m,ints) i
    | (length m) > i = round' divisor (turn divisor (m,ints) i) (i+1)
    | otherwise = (m,ints)

one_round :: Int -> ([Monkey],[(Int,Int)]) -> ([Monkey],[(Int,Int)])
one_round divisor v = round' divisor v 0

n_rounds :: Int -> ([Monkey],[(Int,Int)]) -> Int -> ([Monkey],[(Int,Int)])
n_rounds divisor v 0 = v
n_rounds divisor (m,i) k = let (n,r) = one_round divisor (m,i)
                           in n_rounds divisor (n,my_sum(r)) (k-1)

my_sum' :: [(Int,Int)] -> [(Int,Int)]
my_sum' [] = []
my_sum' [v] = [v]
my_sum' ((i,j):(k,l):vs)
    | i == k = my_sum' ((i,j+l):vs)
    | otherwise = (i,j): (my_sum' ((k,l):vs))

my_sum :: [(Int,Int)] -> [(Int,Int)]
my_sum v = my_sum' (sort v)


process :: [Monkey] -> IO ()
process in_monkeys = do
    let common_divisor = product (map (\v -> test v) in_monkeys)
    putStr "Common divisor is: "
    print common_divisor
    let (mon,its) = n_rounds common_divisor (in_monkeys,[]) 10000
    let m_its = my_sum its
    putStrLn "Result of computation PartII:"
    print $ product $ take 2 $ reverse $ sort (map (\v -> snd v) m_its)

main :: IO ()
main = do
    input <- raw_input
    parsed <- monkey_parse input
    process parsed
