{-
  Author: Mark Jansen
  Date: 09-03-2023

  Summary:
  This program solves simple, empty and difficult sudoku's,
  and indentifies sudoku's that can't be solved due to
  being inconsistent with the sudoku rules.

  Use case:
  - Compile the file with the make folder.
  - Type in terminal: ./SudokuSolver <file-name>
  - Result should be printed to standard output;
  - If not check if you meet the dependencies.
-}

import System.Environment
import Data.List
import Data.Ord

type Row = Int
type Column = Int
type Value = Int
type Grid = [[Value]] -- Only used to read/write from/to a file.
type Sudoku = (Row,Column) -> Value
type Constraint = (Row, Column, [Value])
type Node = (Sudoku, [Constraint])

positions :: [Int]
positions = [1..9]

values :: [Value]
values = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

centerOfBlocks :: [Int]
centerOfBlocks = [2, 5, 8]

{-
  Transform Type Sudoku into Type Grid.

  Input: Finished sudoku of type Sudoku.

  Output: Sudoku of Type Grid.
-}
sud2grid :: Sudoku -> Grid
sud2grid s = [[s (r, c) | c <- positions] | r <- positions]

grid2sud :: Grid -> Sudoku
grid2sud gr = \(r, c) -> pos gr (r, c)
  where pos :: [[a]] -> (Row,Column) -> a
        pos gr (r, c) = (gr !! (r - 1)) !! (c - 1)

{-
  Read a file-sudoku with a Grid like format into a Sudoku.

  String: String of file containing the Sudoku.

  Output: Sudoku of type Sudoku.
-}
readSudoku :: String -> IO Sudoku
readSudoku filename =
    do stringGrid <- readFile filename
       return $ (grid2sud . splitStringIntoGrid) stringGrid
       where splitStringIntoGrid = map (map readint . words) . lines
             readint x = read x :: Int

{-
   Prints a Sudoku to the terminal by transforming it to a grid first.
   Do not modify this, or your tests will fail.

   Sudoku: Sudoku of type Sudoku.

   Side-effect: Print to standard output.
-}
printSudoku :: Sudoku -> IO ()
printSudoku = putStr . unlines . map (unwords . map show) . sud2grid

{-
  Helper to parse command-line arguments.

  [String] : Arguments from Standard Input.

  Output: String containing the name.
-}
getSudokuName :: [String] -> String
getSudokuName [] = error "Filename of sudoku as first argument."
getSudokuName (x:_) = x

{-
  Check what numbers are available in given row.

  Sudoku: Sudoku of Type Sudoku.
  Row: Row of type int.

  Output: List of possible choices of type int.
-}
freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r =  positions \\ [s(r,i) | i <- positions]

{-
  Check what numbers are available in given column.

  Sudoku: Sudoku of Type Sudoku.
  Column: Column of type int.

  Output: List of possible choices of type int.
-}
freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = positions \\ [s(i,c) | i <- positions]

{-
  Find block position.

  [[Int]]: blocks()
  Tuple: Position of block.

  Output: List of possible choices of type int.
-}
findBlock :: [[Int]] -> (Value, Value) -> Value
findBlock (x:xs) (r, i) =
   if r `elem` x
      then i
   else
      findBlock xs (r, i + 1)

{-
  Check what numbers are available in given block.

  Sudoku: Sudoku of Type Sudoku.
  Tuple: Position of block.

  Output: List of possible choices of type int.
-}
freeInSubgrid :: Sudoku -> (Row, Column) -> [Value]
freeInSubgrid s (r, c) = do
   let start_r = findBlock blocks (r, 0) * 3
   let start_c = findBlock blocks (c, 0) * 3
   positions \\ [s(i,j) | i <- [start_r + 1 .. start_r + 3], j <- [start_c + 1 .. start_c + 3]]

{-
   Find the common elements in three lists.

   3x [a] = List containing numbers that can be chosen.

   Output: The definit choices at a location.
-}
commonElements :: Eq a => [a] -> [a] -> [a] -> [a]
commonElements xs ys zs = [ x | x <- xs, x `elem` ys, x `elem` zs ]

{-
    Find the free numbers at a position.

    Sudoku: Sudoku of type Sudoku.
    (Row, Column): Location.

    Output: Possible numbers at location.
-}
freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r, c) = do
   let inRow = freeInRow s r
   let inCol = freeInColumn s c
   let inGrid = freeInSubgrid s (r, c)
   commonElements inRow inCol inGrid

{-
  Give the locations in soduku which are empty.

  Sudoku: Sudoku of Type Sudoku.

  Output: All coordinates.
-}
openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [(x, y) | x <- positions, y <- positions, s(x,y) == 0]

{-
  Check if a Row is valid.
-}
rowValid :: Sudoku -> Row -> Bool
rowValid s r = do
   let inRow = [s(r,i) | i <- positions]
   all (\x -> length (filter (==x) inRow) <= 1) [1..9]

{-
  Check if Column is valid.
-}
colValid :: Sudoku -> Column -> Bool
colValid s c = do
   let inCow = [s(i,c) | i <- positions]
   all (\x -> length (filter (==x) inCow) <= 1) [1..9]

{-
   Find the numbers in a grid.
-}
makeSubGrid :: Sudoku -> (Row, Column) -> [Value]
makeSubGrid s (r, c) = do
   let start_r = findBlock blocks (r, 0) * 3
   let start_c = findBlock blocks (c, 0) * 3
   [s(i,j) | i <- [start_r + 1 .. start_r + 3], j <- [start_c + 1 .. start_c + 3]]

{-
  Check if the subgrid is valid.
-}
subGridValid :: Sudoku -> (Row,Column) -> Bool
subGridValid s (r, c) = do
   let inBlock = makeSubGrid s (r, c)
   all (\x -> length (filter (==x) inBlock) <= 1) [1..9]

{-
  Check if the list contains only True.
-}
allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue xs = and xs

{-
  Check if the sudoku is consistent with the rules.
-}
consistent :: Sudoku -> Bool
consistent s = do
   let result = allTrue [subGridValid s (x, x) && colValid s x && rowValid s x | x <- positions]
   result

{-
  Sort list of Constraints by amount of items in list
  in ascending order.
-}
sortByLeastValues :: [(Row, Column, [Value])] -> [(Row, Column, [Value])]
sortByLeastValues = sortBy (\(_, _, vs1) (_, _, vs2) -> compare (length vs1) (length vs2))

{-
  Calculate the list of constraint of the sudoku.
-}
constraints :: Sudoku -> [Constraint]
constraints s = do
   let result = [(fst x, snd x, freeAtPos s x) | x <- openPositions s]
   sortByLeastValues result

{-
  Solve normal 9x9 sudoku's.

  Output: sud if consistent,
          else return Can't be solved.
-}
solveSudoku :: Sudoku -> Sudoku
solveSudoku sud
    | consistent (head (solutions sud)) = head (solutions sud)
    | otherwise = error "Can't be solved!"

{-
  Calculate the possible solution(s) for the Sudoku
  by splitting the sudoku when multiple choices
  are possible.
-}
solutions :: Sudoku -> [Sudoku]
solutions sud
    | openPositions sud /= [] =
        concat [solutions (extend sud (r, c, v)) | v <- xs]
    | otherwise = [sud]
    where (r, c, xs) = head (constraints sud)

{-
Extends a sudoku with a value at (row, column).
-}
extend :: Sudoku -> (Row, Column, Value) -> Sudoku
extend sud (r, c, v) (i, j) = if r == i && c == j then v else sud (i, j)

main :: IO ()
main =
    do args <- getArgs
       sud <- (readSudoku . getSudokuName) args
       if consistent sud
         then do
            let result = solveSudoku sud
            if consistent result
               then printSudoku result
            else error "Can't be solved"
       else
         error "Can't be solved, because given sudoku is not consistent from the start."