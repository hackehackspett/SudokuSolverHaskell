# SudokuSolverHaskell

Install QuickCheck:
> cabal install QuickCheck

To solve a sudoku, use the following command to read it from file with correct format (see SudokuFileExample.txt): 
> solve "SudokuFileExample.txt"

To check if one sudoku is a valid solution of another:
> sudoku1 <- reader "Sudoku1.txt"

> sudoku2 <- reader "Sudoku2.txt"

> isSolution sudoku1 sudoku2
