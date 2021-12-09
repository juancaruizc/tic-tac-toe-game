module Main where

import Data.List

--Defines the square is used by a move type or it is empty
data Square
  = Occupied MoveType
  | Empty

--Defines the type of move can be either X or O
data MoveType
  = X
  | O

--Renders and X or O if it is occupied or empty if empty string
instance Show Square where
  show (Occupied X) = "X"
  show (Occupied O) = "O"
  show Empty = " "

--Renders an X or O if is is marked as occupied
instance Eq Square where
  Occupied X == Occupied X = True
  Occupied O == Occupied O = True
  Empty == Empty = True
  _ == _ = False

horizontalRow :: String
horizontalRow = "---------"

displayRow :: [Square] -> String
displayRow row = intercalate " | " $ fmap show row

--This function renders the table
renderTable :: [Square] -> IO ()
renderTable table = do
  --Top Row
  putStrLn $ displayRow top
  putStrLn horizontalRow
  --Middle Row
  putStrLn $ displayRow middle
  putStrLn horizontalRow
  --Bottom Row
  putStrLn $ displayRow bottom
  where
    --Defining what top middle and bottom squares are and what Squares belong to each
    top = take 3 table
    middle = drop 3 . take 6 $ table
    bottom = drop 6 table

--Defining the Squares/squares
tableSquare :: String -> Maybe Int
tableSquare "1" = Just 0
tableSquare "2" = Just 1
tableSquare "3" = Just 2
tableSquare "4" = Just 3
tableSquare "5" = Just 4
tableSquare "6" = Just 5
tableSquare "7" = Just 6
tableSquare "8" = Just 7
tableSquare "9" = Just 8

--This function starts the game, passing in the table to be rendered
start :: MoveType -> [Square] -> IO ()
start movetype table = do
  renderTable table

main :: IO ()
main = do
  let newTable = replicate 9 Empty
  start X newTable