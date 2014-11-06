{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import System.Process (system)
import Text.Printf (printf)

import qualified Data.List
import qualified Data.Map

data Value = On | Off
  deriving (Eq, Show)

type CellMap = Data.Map.Map (Int, Int) Value

data Board = Board Int Int CellMap
  deriving (Eq, Show)


printBoard :: Board -> IO ()
printBoard (Board x y cells) = do
    forM_ [1 .. x] $ \x_ -> do
        forM_ [1 .. y] $ \y_ -> do
            case (Data.Map.lookup (x_, y_) cells) of
                Just On -> putStr "O"
                Just Off -> putStr "."
                Nothing -> error $ printf "Could not lookup index (%d, %d)" x_ y_

        putStr "\n"


readBoard :: IO Board
readBoard = do
    content <- getContents
    let header = Data.List.takeWhile (/= '\n') content
    let (x, y) :: (Int, Int) = read header

    let footer = Data.List.dropWhile (/= '\n') content
    let filtered = filter (\e -> elem e ['.', 'O']) footer
    let values = map convert filtered

    let indexes = [ (x_, y_) | x_ <- [1 .. x] , y_ <- [1 .. y] ]

    return $ Board x y (Data.Map.fromList $ zip indexes values)

  where convert 'O' = On
        convert '.' = Off
        convert other = error $ printf "Unexpected char: '%c'" other


rules :: [(Value, Int)]
rules = [ (On, 2), (On, 3), (Off, 3) ]


allNeighbors :: (Int, Int) -> Board -> [Value]
allNeighbors (x, y) (Board _ _ cells) =
    [ value
    | x_ <- [x - 1, x, x + 1]
    , y_ <- [y - 1, y, y + 1]

    , not (x == x_ && y == y_)
    , Data.Map.member (x_, y_) cells

    , value <- [cells Data.Map.! (x_, y_)] ]


iterateBoard :: Board -> Board
iterateBoard oldBoard@(Board x y oldCells) = Board x y newCells
  where newCells = Data.Map.mapWithKey computeCell oldCells

        computeCell (x_, y_) value | elem (value, onNeighbors (x_, y_)) rules = On
        computeCell _ _ = Off

        onNeighbors (x_, y_) = length $ filter (== On) (allNeighbors (x_, y_) oldBoard)


main :: IO ()
main = do board <- readBoard
          go (0 :: Integer) board

  where go count board = do putStrLn $ printf "Iteration #%d" count
                            printBoard board

                            threadDelay (500 * 1000 :: Int)
                            _ <- system "clear"

                            go (count + 1) (iterateBoard board)
