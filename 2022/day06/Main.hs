import AocLib
import Data.List

not_uniq :: String -> Bool
not_uniq s = (length (nub s)) /= (length s)

move :: String -> String -> String
move (h:hs) (s:ss) 
    | not_uniq (hs ++ [s]) = h : (move (hs ++ [s]) ss)
    | otherwise = (h:hs) ++ [s]

main :: IO ()
main = do
    dat <- read_input
    print dat
    print "End of message:"
    print (map (\i -> length (move (take 4 i) (drop 4 i))) dat)
    print "Start of message:"
    print (map (\i -> length (move (take 14 i) (drop 14 i))) dat)

