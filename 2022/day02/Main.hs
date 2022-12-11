import System.IO
import Data.List.Split
import Control.Monad
import System.Environment

data Gesture = A | B | C deriving (Show, Eq)
data Choice = X | Y | Z deriving (Show, Eq)

parse_op :: String -> Gesture
parse_op "A" = A --rock
parse_op "B" = B --paper
parse_op "C" = C --scissors

parse_ch :: String -> Choice
parse_ch "X" = X -- loose
parse_ch "Y" = Y -- draw
parse_ch "Z" = Z -- win 

tuples :: [a] -> [(a,a)]
tuples [] = []
tuples (u:v:hs) = (u,v) : (tuples hs)

hand_points :: Gesture -> Int
hand_points A = 1
hand_points B = 2
hand_points C = 3

wins :: Gesture -> Gesture -> Int
wins A A = 3
wins B B = 3
wins C C = 3
wins A B = 6
wins B C = 6
wins C A = 6
wins _ _ = 0

get_hand :: (Gesture, Choice) -> (Gesture,Gesture)

get_hand (A,X) = (A,C)
get_hand (B,X) = (B,A)
get_hand (C,X) = (C,B)

get_hand (v,Y) = (v,v)

get_hand (A,Z) = (A,B)
get_hand (B,Z) = (B,C)
get_hand (C,Z) = (C,A)

calculate :: (Gesture, Gesture) -> Int
calculate (op, ch) = (wins op ch) + (hand_points ch)

records :: String -> [[String]]
records l = (init [words xs | xs <- (splitOn "\n" l)])


parse_game :: [String] -> (Gesture, Choice)
parse_game (x:y:[]) = ((parse_op x),(parse_ch y))

parse_games :: [[String]] -> [(Gesture, Choice)]
parse_games list = [ parse_game x | x <- list ]

main = do
        args <- getArgs
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        let games = map get_hand ((parse_games . records) contents)
        print games
        let calc_games = map calculate games
        print calc_games 
        print (sum calc_games)
