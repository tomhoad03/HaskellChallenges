module Tests where

import Challenges ( Placement, WordSearchGrid, LamExpr(LamApp, LamAbs, LamVar, LamMacro), LamMacroExpr(LamDef),
                    createWordSearch, solveWordSearch, prettyPrint, parseLamMacro, cpsTransform, innerRedn1, outerRedn1, compareInnerOuter )

main :: IO ()
main = do putStrLn "\nChallenge 1 - Creating a word search:"
          a <- testCreateWordSearch
          printGrid a
          putStrLn "\nChallenge 2 - Solving a word search:"
          b <- testSolveWordSearch a
          print b
          putStrLn "\nChallenge 3 - pretty printing a lamda expression:"
          c <- testPrettyPrint ex3'1 -- (\\x1 -> x1) \\x1 -> x1
          print c
          d <- testPrettyPrint ex3'2 -- \\x1 -> x1 \\x1 -> x1
          print d
          e <- testPrettyPrint ex3'3 -- "def F = \\x1 -> x1 in \\x2 -> x2 F"
          print e
          f <- testPrettyPrint ex3'4 -- "def F = \\x1 -> x1 in \\x2 -> F x2"
          print f
          g <- testPrettyPrint ex3'5 -- "def A = x1 and def B = x2 in (A B) x3"
          print g
          putStrLn "\nChallenge 4 - parsing a lamda expression:"
          print (testParseLamMacro ex4'1) -- Just (LamDef [] (LamApp (LamApp (LamVar 1) (LamVar 2)) LamVar 3)))
          print (testParseLamMacro ex4'2) -- Just (LamDef [] (LamApp (LamVar 1) (LamApp (LamVar 2) (LamMacro "F"))))
          print (testParseLamMacro ex4'3) -- Just (LamDef [("F", LamAbs 1 (LamVar 1))] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F"))))
          print (testParseLamMacro ex4'4) -- Nothing (not in grammar)
          print (testParseLamMacro ex4'5) -- Nothing (repeated definition)
          print (testParseLamMacro ex4'6) -- Nothing (macro body not closed)
          putStrLn "\nChallenge 5 - converting a CPS lamda expression:"
          h <- testCPSTransform (LamDef [] ex5'1) -- \\x3 -> (\\x6 -> x6 x1) (\\x4 -> (\\x7 -> x7 x2) (\\x5 -> x4 x5 x3)) 
          print h
          i <- testCPSTransform ex5'2 -- def F = \\x3 -> x3 (\\x1 -> (\\x4 -> x4 x1)) in (\\x5 -> x5 x2)
          print i
          j <- testCPSTransform ex5'3 -- def F = \\x2 -> x2 (\\x1 -> (\\x3 -> x3 x1)) in F
          print j
          k <- testCPSTransform ex5'4 -- def F = \\x2 -> x2 (\\x1 -> (\\x3 -> x3 x1)) in \\x4 -> F (\\x5 -> F (\\x6 -> x5 x6 x4))
          print k
          putStrLn "\nChallenge 6 - counting and comparing reductions to CPS:\n"

-- Challenge 1 and 2:
-- Test creating a word search
testCreateWordSearch :: IO WordSearchGrid
testCreateWordSearch = do createWordSearch exWords1 0.5

-- Test solving a word search
testSolveWordSearch :: WordSearchGrid -> IO [([Char], Maybe Placement)]
testSolveWordSearch grid = do return (solveWordSearch exWords1 grid)

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
testPrettyPrint :: LamMacroExpr -> IO [Char]
testPrettyPrint expr = do return (prettyPrint expr)

-- Test parsing a lamda expression
testParseLamMacro :: String -> Maybe LamMacroExpr
testParseLamMacro macro = do parseLamMacro macro

-- Challenge 5 and 6:
-- Test pretty printing a lamda expression
testCPSTransform :: LamMacroExpr -> IO LamMacroExpr
testCPSTransform expr = do return (cpsTransform expr)

-- Testing examples
-- Challenge 1 and 2 testing grid and word
exGrid1 :: [[Char]]
exGrid1 = ["HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG"]
exWords1 :: [[Char]]
exWords1 = ["HASKELL","STRING","STACK","MAIN","METHOD"]

-- Challenge 3 testing expressions
ex3'1 :: LamMacroExpr
ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
ex3'2 :: LamMacroExpr
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
ex3'3 :: LamMacroExpr
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
ex3'4 :: LamMacroExpr
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))
ex3'5 :: LamMacroExpr
ex3'5 = LamDef [ ("A", LamVar 1) , ("B", LamVar 2) ] (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))

-- Challenge 4 testing expressions
ex4'1 :: [Char]
ex4'1 = "x1 x2 x3"
ex4'2 :: [Char]
ex4'2 = "x1 (x2 F)"
ex4'3 :: [Char]
ex4'3 = "def F = \\x1 -> x1 in \\x2 -> x2 F"
ex4'4 :: [Char]
ex4'4 = "def F = \\x1 -> x1 (def G = \\x1 -> x1 in x1) in \\x2 -> x2"
ex4'5 :: [Char]
ex4'5 = "def F = \\x1 -> x1 in def F = \\x2 -> x2 x1 in x1"
ex4'6 :: [Char]
ex4'6 = "def F = x1 in F"

-- Challenge 5 testing expressions
exId :: LamExpr
exId = LamAbs 1 (LamVar 1)
ex5'1 :: LamExpr
ex5'1 = LamApp (LamVar 1) (LamVar 2)
ex5'2 :: LamMacroExpr
ex5'2 = LamDef [ ("F", exId) ] (LamVar 2) 
ex5'3 :: LamMacroExpr
ex5'3 = LamDef [ ("F", exId) ] (LamMacro "F")
ex5'4 :: LamMacroExpr
ex5'4 = LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F"))