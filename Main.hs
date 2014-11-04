module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM)
import Text.Printf (printf)

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
    let filtered = filter (\e -> elem e ['.', 'O']) content
    let values = map convertCharToValue filtered
    
    -- As a hack, we use the size of the first line to determine the size of the square
    let size = length $ head $ lines content

    let indexes = [ (x, y) | x <- [1 .. size] , y <- [1 .. size] ]

    return $ Board size size (Data.Map.fromList $ zip indexes values)
  where convertCharToValue 'O' = On
        convertCharToValue '.' = Off
        convertCharToValue other = error $ printf "Unexpected char: '%c'" other

neighbors :: (Int, Int) -> Board -> [Value]
neighbors (x, y) (Board maxX maxY cells) = [ cells Data.Map.! (x + dx, y + dy)
                                           | dx <- [1, 0, -1]
                                           , dy <- [1, 0, -1]
                                           , (dx, dy) /= (0, 0)
                                           , newX <- [x + dx]
                                           , newY <- [y + dy]
                                           , newX > 0 , newY > 0
                                           , newX <= maxX , newY <= maxY ]


onNeighbors :: (Int, Int) -> Board -> Int
onNeighbors (x, y) board = length ons
  where ons = filter (== On) (neighbors (x, y) board)

nextBoard :: Board -> Board
nextBoard oldBoard@(Board oldX oldY oldCells) = Board oldX oldY newCells
  where newCells = Data.Map.mapWithKey convertCell oldCells

        convertCell (x, y) On = case onNeighbors (x, y) oldBoard of
            2 -> On
            3 -> On
            _ -> Off

        convertCell (x, y) Off = case onNeighbors (x, y) oldBoard of
            3 -> On
            _ -> Off


countStart :: Integer
countStart = 0

countDelay :: Int
countDelay = 100 * 1000

main :: IO ()
main = do board <- readBoard
          putStrLn "Starting with:"
          printBoard board
          threadDelay countDelay

          go countStart board
  where go count board = do let newBoard = nextBoard board
                            putStrLn $ printf "Iteration #%d" count
                            printBoard newBoard
                            threadDelay countDelay
                            go (count + 1) newBoard
