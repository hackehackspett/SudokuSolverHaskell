import Data.Char
import Data.Maybe
import System.FilePath
import Data.List
import Test.QuickCheck

--Given
data Sudoku = Sudoku [[Maybe Int]]
            deriving (Eq, Show)

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

--Makes a sudoku with no filled in cells
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 makeNothing)
    where
        makeNothing = replicate 9 Nothing

--Checks if the sudoku is correctly formatted
isSudoku :: Sudoku -> Bool
isSudoku sud
    | isSudokuColumns sud && isSudokuRows sud && isSudokuNumbers sud = True
    | otherwise = False

--Checks if the number of columns is correct
isSudokuColumns :: Sudoku -> Bool
isSudokuColumns sud
    | length (rows (sud)) == 9 = True
    | otherwise = False

--Checks if the length of the rows is correct
isSudokuRows :: Sudoku -> Bool
isSudokuRows sud = isSudokuRows' (length (rows (sud)) - 1) (rows sud)

isSudokuRows' :: Int -> [[Maybe Int]] -> Bool
isSudokuRows' n l
    | (n > 0 && n < 10) && (length (l !! (n)) == 9) = isSudokuRows' (n-1) l
    | n == 0 && (length (l !! n) == 9) = True
    | otherwise = False

--Checks if sudoku contains correct values
isSudokuNumbers :: Sudoku -> Bool
isSudokuNumbers sud
    | isSudokuNumbers' (length (rows (sud)) - 1) (rows sud) = True
    | otherwise = False
    
isSudokuNumbers' :: Int -> [[Maybe Int]] -> Bool
isSudokuNumbers' n l
    | n > 0 && (all isSudokuNumbers'' (l !! (n))) 
    = isSudokuNumbers' (n-1) l
    | n == 0 && (all isSudokuNumbers'' (l !! (n))) = True
    | otherwise = False
    
isSudokuNumbers'' :: Maybe Int -> Bool
isSudokuNumbers'' i
    | i == Nothing || ((fromJust i >= 1) && (fromJust i <= 9)) = True
    | otherwise = False
    
--Checks if sudoku is filled in
isSolved :: Sudoku -> Bool
isSolved sud = all (/= Nothing)(concat (rows sud))
 
-- Prints sudoku as rows of numbers and dots
printSudoku :: Sudoku -> IO ()
printSudoku sud = putStr (unlines(map printSudoku' (rows sud)))
    where
        printSudoku' [] = []
        printSudoku' (x:xs)
            | x == Nothing = "." ++ printSudoku' xs
            | otherwise = [intToDigit (fromJust x)] ++ printSudoku' xs

--Read files and converts the text to sudoku
reader :: FilePath -> IO Sudoku
reader fajl = 
    do f1 <- readFile fajl
       return $ Sudoku 
              $ map toMaybeInt 
              $ concat 
              $ group  
              $ tails(lines f1) !! 0
    where 
        toMaybeInt [] = []
        toMaybeInt (x:xs)
            | x == '.' = [Nothing] ++ toMaybeInt xs
            | (digitToInt x >= 1 && digitToInt x <= 9) 
            = ([Just (digitToInt x)]) ++ toMaybeInt xs

--Instructions for generating a sudoku cell
cell :: Gen (Maybe Int)
cell = frequency
    [  (1, do r <- choose (1, 9)
              return (Just r))
    ,  (9, return Nothing)
    ]
    
--Make Sudoku an instance of the class Arbitrary
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

--Checks if generated sudokus are correctly formatted
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

--Creates a type Block which is a row, column or square in a Sudoku
type Block = [Maybe Int]

--Checks if a block has just one of each number 1-9 or any number of Nothing
--and the correct amount of elements (9)
isOkayBlock :: Block -> Bool
isOkayBlock block
    |(length block == 9) && 
    all (==1)(map length (tail (group (sort (Nothing : block))))) = True
    | otherwise = False
--To make sure that no Just (number) is lost a Nothing is always added
--which is then removed by the tail function

--Creates a list of blocks for all rows, columns and squares
blocks :: Sudoku -> [Block]
blocks sud = blockRows sud ++ blockCols sud ++ blockSquares sud 3

blockRows :: Sudoku -> [Block]
blockRows = rows 

blockCols :: Sudoku -> [Block]
blockCols sud = transpose (rows sud)

blockSquares :: Sudoku -> Int -> [Block]
blockSquares sud i
    | i < 7
    =  blockSquares' sud 3 
    ++ blockSquares' sud 6
    ++ blockSquares' sud 9 
    ++ blockSquares (Sudoku (map (drop 3) (rows sud))) (i + 3)
    |  otherwise 
    =  blockSquares' sud 3 
    ++ blockSquares' sud 6
    ++ blockSquares' sud 9

blockSquares' :: Sudoku -> Int -> [Block]
blockSquares' sud i 
    = [concat (map (take 3) (take 3 (drop (i - 3) (rows sud))))]

--Tests to see that all blocks are valid.
isOkay :: Sudoku -> Bool
isOkay sud = all (== True) (map isOkayBlock (blocks sud))

--Test for quickCheck
prop_Blocks :: Sudoku -> Bool
prop_Blocks sud = all (== True) ([all (== 9) (map length(blocks sud))]
    ++ [27 == (length (blocks sud))])

--Creates the type Pos which is a coordinate in a Sudoku
type Pos = (Int,Int)

--Picks out the first empty coordinate from a given Sudoku
blank :: Sudoku -> Pos
blank sud 
    | (indexEmpty /= Nothing) = (coordinates 0) !! (fromJust (indexEmpty))
    | otherwise = (9, 9)
    where 
        indexEmpty = elemIndex Nothing (concat (rows sud))

--Test to check that the first blank cell really is blank
prop_Blank :: Sudoku -> Bool
prop_Blank sud  
    = ((concat (rows sud)) 
    !! (fromJust ((elemIndex (blank sud) (coordinates 0))))) == Nothing

--Makes all coordinates (with original input 0)
coordinates :: Int -> [Pos]
coordinates i
    | i < 8     = zip [0..8] (replicate 9 i) ++ coordinates (i+1)
    | otherwise = zip [0..8] (replicate 9 i)

--Operator that replaces an element at an index with input (_,a)
(!!=) :: [a] -> (Int,a) -> [a]
[] !!= (_, e) = [e]
xs !!= (i, e) = ys ++ [e] ++ zs
    where 
        ys = take (i) xs
        zs = drop (i+1) xs

--Test to check if the operator works as intended
prop_Operator :: [Int] -> (Int , Int) -> Bool
prop_Operator xs (i, e)
    | i < 0 = (head upxs == e)
    | i > (length xs) = (last upxs == e)
    | otherwise = ((upxs !! i) == e)
    where
        upxs = xs !!= (i, e)


-- Uses above operator to replace empty cells with an input Maybe Int
update :: Sudoku -> Pos -> Maybe Int ->  Sudoku
update sud (y, x) e  
    = Sudoku ((rows sud) !!= (x, ((rows sud) !! x) !!= (y, e)))

--Test to check if updated cell received the correct value
prop_Update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_Update sud (y,x) e 
    = (concat (rows (update sud (y', x') e)))
    !! (fromJust (elemIndex (y',x') (coordinates 0))) == e
    where 
        y' = y `mod` 8
        x' = x `mod` 8

--Solves the Sudoku (somewhat slow)
solver :: Sudoku -> Maybe Sudoku
solver sud
    | not (isOkay sud) = Nothing
    | (isSolved sud) = Just sud
    | otherwise = solver' (blank sud) sud  1

solver' :: Pos -> Sudoku -> Int -> Maybe Sudoku
solver' (y, x) sud i
    | isSolved sud && isOkay sud = Just sud
    | i > 9 || not (isOkay sud)  = Nothing
    | checkNext /= Nothing       = checkNext
    | otherwise = solver' (y, x) sud  (i + 1)
    where
        checkNext = solver' (blank newSud) ((newSud))  1
        newSud    = update sud (y, x) (Just i)

--Reads a Sudoku from a file, solves it and prints it on the terminal. 
solve :: FilePath -> IO ()
solve file = 
    do sud <- reader file
       printSudoku (fromJust (solver sud))

--Checks if a solved sudoku is a valid solution of teh original sudoku 
isSolution :: Sudoku -> Sudoku -> Bool
isSolution sud1 sud2 = (isOkay sud1) && (isSolved sud1)
    && helper (concat (rows sud1)) (concat (rows sud2)) ind ((length ind) -1)
    where 
        ind = (findIndices (/= Nothing) (concat (rows sud2)))

helper :: [Maybe Int] -> [Maybe Int] -> [Int] -> Int -> Bool
helper sud1 sud2 xs i 
    | i == 0 = True
    | (sud1 !! (xs !! i)) == (sud2 !! (xs !! i)) = helper sud1 sud2 xs (i-1)
    | otherwise = False 


--Checks if solver is sound
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isOkay sud ==> isSolution (fromJust (solver sud)) sud

--30 tests of QuickCheck instead of 100
fewerCheck prop = quickCheckWith stdArgs{ maxSuccess = 30 } prop
