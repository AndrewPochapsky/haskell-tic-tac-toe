import Data.List
import Data.Char
import System.IO

data Error = IndexOutOfBounds | TileOccupied
    deriving Show

data GameStatus = XWon | OWon | InProgress
    deriving (Show, Eq)

data Tile = X | O | None
    deriving (Show, Eq)

startingBoard :: [[Tile]] = replicate 3 $ replicate 3 None
comma = ","

-- Game Logic
getGameStatus :: [[Tile]] -> GameStatus
checkHorizontal :: [[Tile]] -> Tile -> Bool
checkVertical :: [[Tile]] -> Tile -> Bool
checkLeftDiagonal :: [[Tile]] -> Tile -> Bool
checkLeftDiagonalHelper :: [[Tile]] -> Tile -> Int -> Bool
checkRightDiagonal :: [[Tile]] -> Tile -> Bool
checkRightDiagonalHelper :: [[Tile]] -> Tile -> Int -> Bool
makeMove :: [[Tile]] -> Tile -> (Int, Int) -> Either Error [[Tile]]
updateBoard :: [[Tile]] -> Tile -> (Int, Int) -> [[Tile]]
getNextTile :: Tile -> Tile

getGameStatus board
    | checkHorizontal board X = XWon
    | checkHorizontal board O = OWon
    | checkVertical board X = XWon
    | checkVertical board O = OWon
    | checkLeftDiagonal board X = XWon
    | checkLeftDiagonal board O = OWon
    | checkRightDiagonal board X = XWon
    | checkRightDiagonal board O = OWon
    | otherwise = InProgress

checkHorizontal [] _ = False
checkHorizontal (row:rows) tile = replicate 3 tile == row || checkHorizontal rows tile

checkVertical board = checkHorizontal (transpose board)

checkLeftDiagonal board tile = checkLeftDiagonalHelper board tile 0
checkLeftDiagonalHelper [] tile _ = True
checkLeftDiagonalHelper (row:rows) tile index =
    row !! index == tile && checkLeftDiagonalHelper rows tile (index + 1)

checkRightDiagonal board tile = checkRightDiagonalHelper board tile 2
checkRightDiagonalHelper [] tile _ = True
checkRightDiagonalHelper (row:rows) tile index =
    row !! index == tile && checkRightDiagonalHelper rows tile (index - 1)

makeMove board tile (x, y)
    | x < 0 || x > 2 || y < 0 || y > 2 = Left IndexOutOfBounds
    | boardValue /= None = Left TileOccupied
    | otherwise = Right $ updateBoard board tile (x, y)
    where boardValue :: Tile = board !! x !! y

updateBoard board tile (x, y) =
    let (before, row:after) = splitAt x board
        (beforeRow, _:afterRow) = splitAt y row
    in before ++ [beforeRow ++ tile : afterRow] ++ after

getNextTile X = O
getNextTile O = X

-- IO Related
stringToMove :: [Char] -> Maybe (Int, Int)
playMoveForPlayer :: Tile -> [[Tile]] -> IO ()

playMoveForPlayer playerTile board = do
    putStrLn ("Player " ++ show playerTile ++ " move: ")
    moveStr <- getLine
    case stringToMove moveStr of
        Just move  -> case makeMove board playerTile move of
            Left err -> do
                        print err
                        playMoveForPlayer playerTile board
            Right board -> if getGameStatus board == InProgress then
                do
                    print board
                    playMoveForPlayer (getNextTile playerTile) board
                else print $ getGameStatus board
        Nothing -> do
            putStrLn "Invalid move, try again"
            playMoveForPlayer playerTile board


stringToMove [x, comma, y] = Just (digitToInt x, digitToInt y)
stringToMove _ = Nothing

main = playMoveForPlayer X startingBoard
