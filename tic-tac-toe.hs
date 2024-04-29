import Data.List

-- Define types for players and the board
data Player = X | O deriving (Eq, Show)
type Board = [[Maybe Player]]

-- Initialize an empty 3x3 board
initialBoard :: Board
initialBoard = replicate 3 (replicate 3 Nothing)

-- Display the board
displayBoard :: Board -> IO ()
displayBoard board = putStrLn $ unlines $ map (intercalate " | " . map (maybe " " show)) boardRows
    where boardRows = [ [ board !! i !! j | j <- [0..2] ] | i <- [0..2] ]

-- Check if the game is over
isGameOver :: Board -> Bool
isGameOver board = any isRowComplete rows || any isRowComplete columns || any isRowComplete diagonals || isBoardFull
    where
        isRowComplete row = all isJust row && allEqual row
        allEqual (x:xs) = all (== x) xs
        allEqual _ = True
        rows = board
        columns = transpose board
        diagonals = [[board !! i !! i | i <- [0..2]], [board !! i !! (2 - i) | i <- [0..2]]]
        isBoardFull = all (all isJust) board

-- Check if a move is valid
isValidMove :: Board -> (Int, Int) -> Bool
isValidMove board (row, col) = row >= 0 && col >= 0 && row < 3 && col < 3 && isNothing (board !! row !! col)

-- Make a move
makeMove :: Board -> (Int, Int) -> Player -> Board
makeMove board (row, col) player = take row board ++ [take col (board !! row) ++ [Just player] ++ drop (col + 1) (board !! row)] ++ drop (row + 1) board

-- Main game loop
ticTacToe :: Board -> Player -> IO ()
ticTacToe board player = do
    displayBoard board
    if isGameOver board
        then putStrLn $ case find isRowComplete (board ++ transpose board ++ diagonals) of
            Just row -> show (fromJust (head row)) ++ " wins!"
            Nothing -> "It's a draw!"
    else do
        putStrLn $ show player ++ "'s turn:"
        putStr "Enter row and column (0-2) separated by space: "
        input <- getLine
        let [row, col] = map read (words input)
        if isValidMove board (row, col)
            then ticTacToe (makeMove board (row, col) player) (if player == X then O else X)
            else do
                putStrLn "Invalid move, please try again."
                ticTacToe board player
    where
        diagonals = [[board !! i !! i | i <- [0..2]], [board !! i !! (2 - i) | i <- [0..2]]]

main :: IO ()
main = ticTacToe initialBoard X
