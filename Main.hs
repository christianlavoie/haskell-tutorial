{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM)
import Data.Maybe (fromJust)
import System.Process (system)
import Text.Printf (printf)

import qualified Data.List
import qualified Data.Map

data Value = On | Off
  deriving (Eq, Show)

data Board = Board Int Int (Data.Map.Map (Int, Int) Value)
  deriving (Eq, Show)


printBoard :: Board -> IO ()
printBoard (Board x y cells) = do
    forM [1 .. x] $ \x -> do
        forM [1 .. y] $ \y -> do
            case (cells Data.Map.! (x, y)) of
                On -> putStr "O"
                Off -> putStr "."

        putStr "\n"
    return ()


readBoard :: IO Board
readBoard = do
    content <- getContents
    let header = Data.List.takeWhile (/= '\n') content
    let (x, y) :: (Int, Int) = read $ head $ lines content

    let footer = Data.List.dropWhile (/= '\n') content
    let filtered = filter (\e -> elem e ['.', 'O']) footer
    let values = map convert filtered

    -- As a hack, we use the size of the first line to determine the size of the square
    let indexes = [ (x_, y_)
                  | x_ <- [1 .. x]
                  , y_ <- [1 .. y] ]

    return $ Board x y (Data.Map.fromList $ zip indexes values)

  where convert 'O' = On
        convert '.' = Off
        convert other = error $ printf "Unexpected char: '%c'" other


rules :: [(Value, Int)]
rules = [ (On, 2), (On, 3), (Off, 3) ]


allNeighbors :: (Int, Int) -> Board -> [Value]
allNeighbors (x, y) (Board maxX maxY cells) =
    [ value
    | x_ <- [x - 1, x, x + 1]
    , y_ <- [y - 1, y, y + 1]

    , not $ and [x == x_, y == y_]
    , Data.Map.member (x_, y_) cells

    , value <- [cells Data.Map.! (x_, y_)] ]


nextBoard :: Board -> Board
nextBoard oldBoard@(Board oldX oldY oldCells) = Board oldX oldY newCells
  where newCells = Data.Map.mapWithKey computeCell oldCells
        computeCell (x, y) value =
            if elem (value, onNeighbors (x, y)) rules
            then On
            else Off

        onNeighbors (x, y) = length $ filter (== On) (allNeighbors (x, y) oldBoard)


main :: IO ()
main = do board <- readBoard
          go (0 :: Integer) board

  where go count board = do putStrLn $ printf "Iteration #%d" count
                            printBoard board

                            threadDelay (100 * 1000 :: Int)
                            system "clear"

                            go (count + 1) (nextBoard board)
