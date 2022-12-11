module Parser (monkey_parse
              ,Operation(Plus,Times,Square)
              ,Monkey(ident,items,ops,test,ifTrue,ifFalse)) where
import Debug.Trace
import Text.Parsec
import Text.Parsec.String

data Operation = Plus Int | Times Int | Square deriving (Show)

data Monkey = Monkey
    { ident        :: Int
    , items     :: [Int]  
    , ops        :: Operation 
    , test      :: Int
    , ifTrue    :: Int
    , ifFalse   :: Int } deriving (Show)


monkeys:: GenParser Char st [Monkey]
monkeys = sepBy monkey (char '\n')

monkey :: GenParser Char st Monkey
monkey = Monkey <$>  pidentifier <* char '\n'
                <*>  pitems <* char '\n'
                <*>  poperation <* char '\n'
                <*>  ptest <* char '\n'
                <*>  pifTrue <* char '\n'
                <*>  pifFalse <* char '\n'

pidentifier :: GenParser Char st Int
pidentifier = do
    string "Monkey "
    id <- (\v -> (read v):: Int) <$> many1 digit
    char ':'
    return id

read_i :: String -> Int
read_i = read

pitems :: GenParser Char st [Int]
pitems = do
    string "  Starting items: "
    it <- (\v -> map read_i v) <$> sepBy (many1 digit) (string ", ")
    return it

poperation :: GenParser Char st Operation
poperation = do
    string "  Operation: new = "
    r <- try parse_plus 
            <|> try parse_times 
            <|> try parse_square 
            <?> "Unable to parse operation"
    return r

parse_plus = do
    string "old + "
    res <- (\v -> Plus (read_i v)) <$> many1 digit
    return res

parse_times = do
    string "old * "
    res <- (\v -> Times (read_i v))<$> many1 digit
    return res
    

parse_square = do
    string "old * old"
    return Square

ptest :: GenParser Char st Int
ptest = do
    string "  Test: divisible by "
    r <- read_i <$> many1 digit
    return r

pifTrue :: GenParser Char st Int
pifTrue = do
    string "    If true: throw to monkey "
    r <- read_i <$> many1 digit
    return r

pifFalse :: GenParser Char st Int
pifFalse = do
    string "    If false: throw to monkey "
    r <- read_i <$> many1 digit
    return r

my_fail :: (Show a) => Either ParseError a -> a 
my_fail x | trace ("my_fail " ++ (show x)) False = undefined
my_fail (Left e) = error (show e)
my_fail (Right f) = f

monkey_parse :: String -> IO [Monkey]
monkey_parse input = do
    let parsed = parse monkeys "unable to Read" input
    case parsed of
        Left e -> fail (show e)
        Right r -> return r
