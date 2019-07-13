module Lib
    ( myMain
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

myMain :: IO ()
myMain = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

getNameAndPrint :: IO ()
getNameAndPrint = do
    putStrLn "What's your name ?"
    name <- getLine
    putStrLn ("HI, " ++ name)

doCompute :: IO ()
doCompute = do
    args <- getArgs
    let x = computeArgs args
    putStrLn (args !! 0 ++ " * " ++ args !! 1 ++ " = ")
    putStrLn $ show x

computeArgs :: [String] -> Int
computeArgs args = (read $ args !! 0) * (read $ args !! 1)

oldMain :: IO ()
oldMain = do
    args <- getArgs
    putStrLn ("Hello, " ++ args !! 0 ++ ".")
    putStrLn ("I hear your favorite color is " ++ args !! 1 ++ ".")

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExprOld :: String -> String
readExprOld input = case parse symbol "lisp" input of
    Left err -> "No match : " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "list" input of
    Left err -> "No match : " ++ show err
    Right val -> "Found value"

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ noneOf "\""
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom