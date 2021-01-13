module Tests where

import Challenges ( Placement, WordSearchGrid, LamExpr(LamApp, LamAbs, LamVar, LamMacro), LamMacroExpr(LamDef),
                    createWordSearch, solveWordSearch, prettyPrint, parseLamMacro )

main :: IO ()
main = do putStrLn "Challenge 1 - Create a word search:"
          a <- testCreateWordSearch
          printGrid a
          putStrLn "\n"
          putStrLn "Challenge 2 - Solve a word search:"
          b <- testSolveWordSearch a
          print b
          putStrLn "\n"
          putStrLn "Challenge 3 - pretty print a lamda expression:"
          c <- testPrettyPrint
          print c
          putStrLn "\n"
          putStrLn "Challenge 4 - parse a lamda expression:"
          print testParseLamMacro

-- Challenge 1 and 2:
-- Test creating a word search
testCreateWordSearch :: IO WordSearchGrid
testCreateWordSearch = do createWordSearch exWords1'1 0.5

-- Test solving a word search
testSolveWordSearch :: WordSearchGrid -> IO [([Char], Maybe Placement)]
testSolveWordSearch grid = do return (solveWordSearch exWords1'1 grid)

-- Print the grid (given)
printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do putStrLn w
                      printGrid ws

-- Create and solve a word search (given)
createAndSolve :: [ [Char] ] -> Double -> IO [ ([Char], Maybe Placement) ]
createAndSolve words maxDensity =   do g <- createWordSearch words maxDensity
                                       let soln = solveWordSearch words g
                                       printGrid g
                                       return soln

-- Challenge 3 and 4:
-- Test pretty printing a lamda expression
testPrettyPrint :: IO [Char]
testPrettyPrint = do return (prettyPrint ex3'4)

-- Test parsing a lamda expression
testParseLamMacro :: Maybe LamMacroExpr
testParseLamMacro = do parseLamMacro ex4'3

-- Testing examples
-- Challenge 1 and 2 testing grids and words
exGrid1'1 :: [[Char]]
exGrid1'1 = ["HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG"]
exWords1'1 :: [[Char]]
exWords1'1 = ["HASKELL","[Char]","STACK","MAIN","METHOD"]
exGrid1'2 :: [[Char]]
exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 :: [[Char]]
exWords1'2 = ["BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE"]

-- Challenge 3 testing expressions
ex3'1 :: LamMacroExpr
ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))) -- (\x1 -> x1) \x1 -> x1
ex3'2 :: LamMacroExpr
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1)))) -- \x1 -> x1 \x1 -> x1
ex3'3 :: LamMacroExpr
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F"))) -- "def F = \\x1 -> x1 in \\x2 -> x2 F"
ex3'4 :: LamMacroExpr
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2))) -- "def F = \\x1 -> x1 in \\x2 -> F x2"

-- Challenge 4 testing expressions
ex4'1 :: [Char]
ex4'1 = "x1 x2 x3" -- Just (LamDef [] (LamApp (LamApp (LamVar 1) (LamVar 2)) LamVar 3)))
ex4'2 :: [Char]
ex4'2 = "x1 (x2 F)" -- Just (LamDef [] (LamApp (LamVar 1) (LamApp (LamVar 2) (LamMacro "F"))))
ex4'3 :: [Char]
ex4'3 = "def F = \\x1 -> x1 in \\x2 -> x2 F" -- Just (LamDef [("F", LamAbs 1 (LamVar 1))] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F"))))
ex4'4 :: [Char]
ex4'4 = "def F = \\x1 -> x1 (def G = \\x1 -> x1 in x1) in \\x2 -> x2" -- Nothing (not in grammar)
ex4'5 :: [Char]
ex4'5 = "def F = \\x1 -> x1 in def F = \\x2 -> x2 x1 in x1" -- Nothing (repeated definition)
ex4'6 :: [Char]
ex4'6 = "def F = x1 in F" -- Nothing (macro body not closed)