import qualified Text.Parsec as Parsec
import Text.Parsec
import Text.Parsec.String
import AocLib
import Text.Printf
-- import Formatting

data Command = Ls | Cd String | Up | ODir String | OFile String Int deriving (Show)

commandFile :: GenParser Char st [Command]
commandFile = Parsec.endBy command (Parsec.char '\n')

command :: GenParser Char st Command
command =  Parsec.try c_up
            <|> Parsec.try c_ls
            <|> try c_cd
            <|> try c_file
            <|> try c_dir
            <?> "invalid input"

c_up :: GenParser Char st Command
c_up = (\_ -> Up) <$> Parsec.string "$ cd .."

c_ls :: GenParser Char st Command
c_ls = (\_ -> Ls) <$> Parsec.string "$ ls"

c_cd :: GenParser Char st Command
c_cd =  do
        Parsec.string "$ cd "
        res <- (\v -> (Cd v)) <$> Parsec.many1 (Parsec.noneOf " \n") 
        return res

r_int :: String -> Int
r_int = read

c_file :: GenParser Char st Command
c_file = do
    size <- (\v -> r_int v) <$> Parsec.many1 Parsec.digit
    Parsec.spaces
    name <- Parsec.many1 (Parsec.noneOf " \n")
    return (OFile name size)

c_dir :: GenParser Char st Command
c_dir = do
    Parsec.string "dir "
    d_name <- Parsec.many1 (Parsec.noneOf " \n")
    return (ODir d_name)


data Fs = File String Int | Dir String Int [Fs] deriving (Show)

_format_one :: String -> (String, Int) -> String
_format_one prefix (name,size)  = printf "%9d %s%s\n" size prefix name

_unpack :: Fs -> (String,Int)
_unpack (File name size) = (name,size)
_unpack (Dir name size _) = (name,size)

_format_iter :: String -> [Fs] -> String
_format_iter prefix [last] = (_format_one (prefix ++ "└── ") (_unpack last)) ++ (_format (prefix ++ "    ") last)
_format_iter prefix (curr:fs) = (_format_one (prefix ++ "│── ") (_unpack curr)) ++ (_format (prefix ++ "│   ") curr) ++ (_format_iter prefix fs)

_format :: String -> Fs -> String
_format _ (File _ _) = ""
_format prefix (Dir _ _ fs) = _format_iter prefix fs

format :: Fs -> String
format (Dir name size fs) = (_format_one "" (name,size)) ++  _format_iter "" fs


fsInsert :: [String] -> (a -> Fs) -> a -> Fs -> Fs
fsInsert _ _ _ (File name val) = (File name val)
fsInsert [d] fn val (Dir name size ls)
    | d == name = (Dir name size ((fn val):ls))
    | otherwise = (Dir name size ls)
fsInsert (d:ds) fn val (Dir name size ls)
    | d == name = (Dir name size (map (fsInsert ds fn val) ls))
    | otherwise = (Dir name size ls)

makeFile :: (String,Int) -> Fs
makeFile (name,size) = File name size

makeDir :: String -> Fs
makeDir name = Dir name 0 []

getSize :: Fs -> Int
getSize (Dir _ v _) = v
getSize (File _ v) = v

fsCount :: Fs -> Fs
fsCount (Dir name size children) = let count = (map fsCount children)
                                   in (Dir name (foldl (\a v -> a + (getSize v)) size count) count)
fsCount (File name size) = File name size

procCommand :: Command -> ([String],Fs) -> ([String],Fs)
procCommand Ls (path,fs) = (path,fs)
procCommand (Cd value) (path,fs) = ((path++[value]),fs)
procCommand Up (path,fs) = ((init path),fs)
procCommand (ODir name) (path,fs) = (path,(fsInsert path makeDir name fs))
procCommand (OFile name size) (path,fs) = (path,(fsInsert path makeFile (name,size) fs))

flattenDirs ::  Fs -> [(String,Int)]
flattenDirs  (File _ _) = []
flattenDirs (Dir name dirSize fs) = [(name,dirSize)] ++ (foldl (\a f -> a ++ (flattenDirs f)) [] fs)

findDirsLower :: Int -> Fs -> [Int]
findDirsLower size fs = map snd (filter (\v -> (snd v)  <= size) (flattenDirs fs))

getRootSize :: Fs -> Int
getRootSize (Dir "/" size _) = size
getRootSize _ = 0

eval :: [Command] -> IO ()
eval comms = do
    let size = 70000000
    let target_size = 30000000
    let root = makeDir "/"
    let readFs = fsCount $ snd $ foldl (\a v -> procCommand v a) ([],root) comms
    putStrLn $ format $ readFs
    let partOne = findDirsLower 100000  readFs
    putStrLn "Part One solution:"
    print partOne
    print $ sum $ partOne
   
    putStrLn "Part two solution:"
    let flat = flattenDirs readFs
    let reminder = size - (getRootSize readFs)
    let within = filter (\s -> (reminder + (snd s)) >= target_size) flat
    print within
    let min = foldl (\a v -> if (snd a) > (snd v) then v else a) (head within) (tail within)
    print min


main :: IO ()
main = do
    cont <- raw_input
    let parsed = Parsec.parse commandFile "unable to Read" cont
    case parsed of
        Left e -> do
            putStrLn "Error parsing"
            print e
        Right r -> eval r

