{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (WordSearchGrid,Placement,Posn,Orientation(..),solveWordSearch, createWordSearch,
    LamMacroExpr(..),LamExpr(..),prettyPrint, parseLamMacro,
    cpsTransform,innerRedn1,outerRedn1,compareInnerOuter) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- We import System.Random - make sure that your installation has it installed - use stack ghci and stack ghc
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random

-- My import statements.
import Data.Function ( on )

instance NFData Orientation
instance NFData LamMacroExpr
instance NFData LamExpr

-- types for Part I
type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read,Generic)

-- types for Parts II and III
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read,Generic)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read,Generic)

-- END OF CODE YOU MUST NOT MODIFY

-- ADD YOUR OWN CODE HERE



-- Challenge 1 --

-- Solves a given word search from a list of given words.
solveWordSearch :: [String] -> WordSearchGrid -> [(String, Maybe Placement)]
solveWordSearch _ [] = error "No word search grid given."
solveWordSearch [] _ = []
solveWordSearch words grid = filteredCheckedWords ++ missingWords
  where
    -- Checks every word in every position.
    gridSize = length grid
    checkedWords = [findWords words grid (x, y) | y <- [0..gridSize - 1], x <- [0..gridSize - 1]]
    filteredCheckedWords = filter (\x -> snd x /= Nothing) checkedWords
    missingWords = [(a, Nothing) | a <- filter (`notElem` map fst filteredCheckedWords) words]

-- Checks every word at a given position.
findWords :: [String] -> WordSearchGrid -> Posn -> (String, Maybe Placement)
findWords words grid (x, y) | null words = ("", Nothing)
                            | (snd foundWord) == Nothing = findWords (tail words) grid (x, y)
                            | otherwise = foundWord
  where
    foundWord = findWord (head words) (x, y) grid

-- Checks if a word that can be found from a given position.
findWord :: String -> Posn -> WordSearchGrid -> (String, Maybe Placement)
findWord word (x, y) grid | length matchedWords == 1 = (word, Just ((x, y), snd (head matchedWords)))
                          | otherwise = (word, Nothing)
  where
    -- Filters out all the words with empty spaces. These are words don't fit on the grid.
    foundWords = zip (getWords (x, y) (length word) grid) [Forward, Back, Up, Down, UpForward, UpBack, DownForward, DownBack]
    matchedWords = matchWords word foundWords

-- Checks if the word matches all the words found from a given position.
matchWords :: String -> [(String, Orientation)] -> [(String, Orientation)]
matchWords word words = filter (\x -> fst x == word) words

-- Gets every word in every orientation from a positon with a given length.
getWords :: Posn -> Int -> WordSearchGrid -> [String]
getWords (x, y) lengthWord grid = map (map (getLetter grid)) foundPositions
  where
    foundPositions = [[(a, y) | a <- [x..(lengthWord + x - 1)]]]
                     ++ [[(a, y) | a <- reverse [(x - (lengthWord - x - 1) - x)..x]]]
                     ++ [[(x, a) | a <- reverse [(y - (lengthWord - y - 1) - y)..y]]]
                     ++ [[(x, a) | a <- [y..(lengthWord + y - 1)]]]
                     ++ [zip [a | a <- [x..(lengthWord + x - 1)]] [b | b <- reverse [(y - (lengthWord - y - 1) - y)..y]]]
                     ++ [zip [a | a <- reverse [(x - (lengthWord - x - 1) - x)..x]] [b | b <- reverse [(y - (lengthWord - y - 1) - y)..y]]]
                     ++ [zip [a | a <- [x..(lengthWord + x - 1)]] [b | b <- [y..(lengthWord + y - 1)]]]
                     ++ [zip [a | a <- reverse [(x - (lengthWord - x - 1) - x)..x]] [b | b <- [y..(lengthWord + y - 1)]]]
                    
-- Gets a letter from the grid when given its position.
getLetter :: WordSearchGrid -> Posn -> Char
getLetter grid (x, y) | x > length grid - 1 || y > length grid - 1 = ' '
                      | x < 0 || y < 0 = ' '
                      | otherwise = (grid !! y) !! x

-- Challenge 1 and 2 testing grids and words.
exGrid1'1 :: [[Char]]
exGrid1'1 = ["HAGNIRTSH" , "SACAGETAK", "GCSTACKEL","MGHKMILKI","EKNLETGCN","TNIRTLETE","IRAAHCLSR","MAMROSAGD","GIZKDDNRG"]
exWords1'1 :: [[Char]]
exWords1'1 = ["HASKELL","STRING","STACK","MAIN","METHOD"]
exGrid1'2 :: [[Char]]
exGrid1'2 = ["ROBREUMBR","AURPEPSAN","UNLALMSEE","YGAUNPYYP","NLMNBGENA","NBLEALEOR","ALRYPBBLG","NREPBEBEP","YGAYAROMR"]
exWords1'2 :: [[Char]]
exWords1'2 = ["BANANA", "ORANGE", "MELON", "RASPBERRY","APPLE","PLUM","GRAPE"]

exGrid1'3 :: [[Char]]
exGrid1'3 = ["YBCCY", "YXCAZ", "ABZYX", "XZYBC", "XYCAB"]
exWords1'3 :: [[Char]]
exWords1'3 = ["ABC", "XYZ"]



-- Challenge 2 --

-- Produces a solvable word search grid.
createWordSearch :: [ String ] -> Double -> IO WordSearchGrid
createWordSearch [] _ = error "No words were given."
createWordSearch words density = do let gridSize = findSize density words
                                    splitGrid gridSize words

-- Calculates the grid size such that the density is less than the one given.
findSize :: Double -> [String] -> Int
findSize expectedDensity words = max minWidth longestWord 
  where
    minWidth = ceiling $ sqrt $ fromIntegral (numLetters words) / expectedDensity
    longestWord = maximum $ map length words

-- Finds the total number of letters in the words given.
numLetters :: [String] -> Int
numLetters words = sum [a | a <- map length words]

-- Splits the grid up into rows.
splitGrid :: Int -> [String] -> IO WordSearchGrid
splitGrid gridSize words = do grid <- mergeGrids gridSize words
                              let stringGrid = map snd grid
                              return (map fst (filter (\x -> snd x `mod` gridSize == 0) (init (zip (map (take gridSize) (tails (head [stringGrid]))) [0..]))))

-- Merges the grid of word positions with a random grid.
mergeGrids :: Int -> [String] -> IO [(Posn, Char)]
mergeGrids gridSize words = do wordGrid <- positionWords gridSize words
                               randGrid <- randomGrid gridSize words gridSize
                               return (map head (groupBy (\x y -> fst x == fst y) (sortBy (compare `on` snd) (wordGrid ++ concat randGrid))))
                               -- This part required me to import Data.Function which is part of the standard prelude.

-- Generates a grid of positions and random letters.
randomGrid :: Int -> [String] -> Int -> IO [[(Posn, Char)]]
randomGrid _ _ 0 = return []
randomGrid gridSize words rowCount = do x <- randomRow gridSize words (gridSize - rowCount) gridSize
                                        xs <- randomGrid gridSize words (rowCount - 1) 
                                        return (x:xs)

-- Generates a row of positions and random letters.
randomRow :: Int -> [String] -> Int -> Int -> IO [(Posn, Char)]
randomRow _ _ _ 0 = return []
randomRow gridSize words rowCount colCount = do x <- randomPositionLetter (gridSize - colCount, rowCount) words
                                                xs <- randomRow gridSize words rowCount (colCount - 1)
                                                return (x:xs)

-- Generates a tuple of position and a random letter.
randomPositionLetter :: Posn -> [String] -> IO (Posn, Char)
randomPositionLetter position words = do letter <- randomLetter words
                                         return (position, letter)

-- Generates a random letter.
randomLetter :: [String] -> IO Char
randomLetter words = do let a = findUniqueLetters words
                        b <- randomNumber (length a)
                        return (a !! b)

-- Finds the list of unique letters that are allowed to be used to randomly populate the grid.
findUniqueLetters :: [String] -> [Char]
findUniqueLetters words = filter (/= ' ') (map head $ group $ sort (unwords words))

-- Generates a random position on the grid.
randomPosition :: Int -> IO Posn
randomPosition maxSize = do a <- randomNumber maxSize
                            b <- randomNumber maxSize
                            return (a, b)

-- Generates a random number.
randomNumber :: Int -> IO Int
randomNumber maxSize = do getStdRandom $ randomR (0, maxSize - 1)

-- Finds a valid list of positions with letters for every word to lie on the grid.
positionWords :: Int -> [String] -> IO [(Posn, Char)]
positionWords gridSize words = do a <- concat <$> mapM (positionWord gridSize) words
                                  let b = map head $ group $ sort a
                                  let c = map head $ group $ sort $ map fst b
                                  if length b /= length c
                                    then positionWords gridSize words
                                    else return b

-- Finds a valid list of positions for a word to lie on the grid at.
positionWord :: Int -> String -> IO [(Posn, Char)]
positionWord gridSize word = do a <- checkPositions gridSize (length word)
                                if False `elem` fst a
                                  then positionWord gridSize word
                                  else return (zip (snd a) word)

-- Checks if a word will lie on the grid.
checkPositions :: Int -> Int -> IO ([Bool], [Posn])
checkPositions gridSize lengthWord = do a <- findPositions lengthWord
                                        return (map (checkPosition gridSize) a, a)

-- Checks if a position will lie on the grid.
checkPosition :: Int -> Posn -> Bool
checkPosition gridSize (x, y) | x >= gridSize || x < 0 || y >= gridSize || y < 0 = False
                              | otherwise = True

-- Finds the possible positions for a word on the grid.
findPositions :: Int -> IO [(Int, Int)]
findPositions lengthWord = do orientation <- randomOrientation
                              x <- randomNumber 8
                              y <- randomNumber 8
                              let a | orientation == Forward = [(a, y) | a <- [x..(lengthWord + x - 1)]]
                                    | orientation == Back = [(a, y) | a <- reverse [(x - (lengthWord - x - 1) - x)..x]]
                                    | orientation == Up = [(x, a) | a <- reverse [(y - (lengthWord - y - 1) - y)..y]]
                                    | orientation == Down = [(x, a) | a <- [y..(lengthWord + y - 1)]]
                                    | orientation == UpForward = zip [a | a <- [x..(lengthWord + x - 1)]] [b | b <- reverse [(y - (lengthWord - y - 1) - y)..y]]
                                    | orientation == UpBack = zip [a | a <- reverse [(x - (lengthWord - x - 1) - x)..x]] [b | b <- reverse [(y - (lengthWord - y - 1) - y)..y]]
                                    | orientation == DownForward = zip [a | a <- [x..(lengthWord + x - 1)]] [b | b <- [y..(lengthWord + y - 1)]]
                                    | orientation == DownBack = zip [a | a <- reverse [(x - (lengthWord - x - 1) - x)..x]] [b | b <- [y..(lengthWord + y - 1)]]
                                    | otherwise = []
                              return a

-- Generates a random orientation.
randomOrientation :: IO Orientation
randomOrientation = do a <- randomNumber 8
                       return ([Forward, Back, Up, Down, UpForward, UpBack, DownForward, DownBack] !! a)



--- My challenge 2 testing functions.
testCreateWordSearch :: IO WordSearchGrid
testCreateWordSearch = do createWordSearch ["ABC", "XYZ"] 0.5

testSplitGrid :: IO [[Char]]
testSplitGrid = do splitGrid 4 ["ABC", "XYZ"]

testFindSize :: Int
testFindSize = do findSize 0.5 ["ABC", "XYZ"]

testMergeGrids :: IO [(Posn, Char)]
testMergeGrids = do mergeGrids 5 ["ABC", "XYZ"]

testPositionWord :: IO [(Posn, Char)]
testPositionWord = do positionWord 5 "Hello"

testFindPositions :: IO [(Int, Int)]
testFindPositions = do findPositions 5

-- Given testing functions.
createAndSolve :: [ String ] -> Double -> IO [ (String, Maybe Placement) ]
createAndSolve words maxDensity =   do g <- createWordSearch words maxDensity
                                       let soln = solveWordSearch words g
                                       printGrid g
                                       return soln

printGrid :: WordSearchGrid -> IO ()
printGrid [] = return ()
printGrid (w:ws) = do putStrLn w
                      printGrid ws



-- Challenge 3 --

-- Reminder of the data types used in parts 2 and 3
-- data LamMacroExpr = LamDef [(String, LamExpr)] LamExpr deriving (Eq, Show, Read)
-- data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
--                LamAbs Int LamExpr  | LamVar Int deriving (Eq, Show, Read)

prettyPrint :: LamMacroExpr -> String
prettyPrint macroExpr | not (null lamDefs) = readDefExprs lamDefs ++ lamExpr
                      | otherwise = lamExpr
  where
    lamDefs = getDefs macroExpr
    lamExpr = readExpr (getExpr macroExpr) lamDefs
    
-- Gets the lambda definitions from a macro expression.
getDefs :: LamMacroExpr -> [(String, LamExpr)]
getDefs (LamDef defs _) = defs

-- Gets the lambda expression from a lambda definition.
getDefValue :: (String, LamExpr) -> String
getDefValue (value, _) = value

-- Gets the lambda expression from a lambda definition.
getDefExpr :: (String, LamExpr) -> LamExpr
getDefExpr (_, expr) = expr

-- Gets the lambda expression from a macro expression.
getExpr :: LamMacroExpr -> LamExpr
getExpr (LamDef _ expr) = expr

-- Gets the value for a given lambda expression.
getValue :: LamExpr -> [(String, LamExpr)] -> String
getValue lamExpr lamDefs = head $ map fst $ filter (\x -> snd x == lamExpr) lamDefs  

-- Checks if a lambda expression is found in the list of those given in the definition.
checkDefs :: LamExpr -> [(String, LamExpr)] -> Bool
checkDefs lamExpr lamDefs = lamExpr `notElem` map snd lamDefs

-- Prints the lambda expressions in the definition.
readDefExprs :: [(String, LamExpr)] -> String
readDefExprs (x:xs) | not (null xs) = exprString ++ " and " ++ readDefExprs xs
                    | otherwise = exprString ++ " in "
  where
    exprString = "def " ++ getDefValue x ++ " = " ++ readExpr (getDefExpr x) []

-- Prints a lambda expression.
readExpr :: LamExpr -> [(String, LamExpr)] -> String

-- Prints macro and variable expressions.
readExpr (LamMacro lamValue) _ = lamValue
readExpr (LamVar lamNum) _ = "x" ++ show lamNum

-- Prints application expressions that don't require brackets - LamMacro version.
readExpr (LamApp (LamMacro lamValue) lamExpr) [] = readExpr (LamMacro lamValue) [] ++ " " ++ readExpr lamExpr []
readExpr (LamApp (LamMacro lamValue) lamExpr) lamDefs | checkDefs lamExpr lamDefs = readExpr (LamMacro lamValue) lamDefs ++ " " ++ readExpr lamExpr lamDefs
                                                      | otherwise = readExpr (LamMacro lamValue) lamDefs ++ " " ++ getValue lamExpr lamDefs

-- Prints application expressions that don't require brackets - LamVar version.
readExpr (LamApp (LamVar lamNum) lamExpr) [] = readExpr (LamVar lamNum) [] ++ " " ++ readExpr lamExpr []
readExpr (LamApp (LamVar lamNum) lamExpr) lamDefs | checkDefs lamExpr lamDefs = readExpr (LamVar lamNum) lamDefs ++ " " ++ readExpr lamExpr lamDefs
                                                  | otherwise = readExpr (LamVar lamNum) lamDefs ++ " " ++ getValue lamExpr lamDefs

-- Prints application expressions that require brackets.
readExpr (LamApp lamExpr1 lamExpr2) [] = "(" ++ readExpr lamExpr1 [] ++ ") " ++ readExpr lamExpr2 []
readExpr (LamApp lamExpr1 lamExpr2) lamDefs | checkDefs lamExpr1 lamDefs && checkDefs lamExpr2 lamDefs = "(" ++ readExpr lamExpr1 lamDefs ++ ") " ++ readExpr lamExpr2 lamDefs
                                            | not (checkDefs lamExpr1 lamDefs) && checkDefs lamExpr2 lamDefs = getValue lamExpr1 lamDefs ++ " " ++ readExpr lamExpr2 lamDefs
                                            | checkDefs lamExpr1 lamDefs && not (checkDefs lamExpr2 lamDefs) = "(" ++ readExpr lamExpr1 lamDefs ++ ") " ++ getValue lamExpr2 lamDefs
                                            | not (checkDefs lamExpr1 lamDefs) && not (checkDefs lamExpr2 lamDefs) = getValue lamExpr1 lamDefs ++ " " ++ getValue lamExpr2 lamDefs

-- Prints abstraction expressions.
readExpr (LamAbs lamNum lamExpr) [] = "\\x" ++ show lamNum ++ " -> " ++ readExpr lamExpr []
readExpr (LamAbs lamNum lamExpr) lamDefs | checkDefs lamExpr lamDefs = "\\x" ++ show lamNum ++ " -> " ++ readExpr lamExpr lamDefs
                                         | checkDefs lamExpr lamDefs = "\\x" ++ show lamNum ++ " -> " ++ getValue lamExpr lamDefs
-- Currently printing double backslashes in abstraction expressions - read up on escaping this.



-- Challenge 3 testing functions.
testGetValue :: String
testGetValue = getValue (LamAbs 1 (LamVar 1)) [("F", LamAbs 1 (LamVar 1))]

testCheckDefs :: Bool
testCheckDefs = checkDefs (LamAbs 1 (LamVar 1)) [("F", LamAbs 1 (LamVar 1))]

-- Challenge 3 testing expressions.
ex3'1 :: LamMacroExpr
ex3'1 = LamDef [] (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
ex3'2 :: LamMacroExpr
ex3'2 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
ex3'3 :: LamMacroExpr
ex3'3 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamVar 2) (LamMacro "F")))
ex3'4 :: LamMacroExpr
ex3'4 = LamDef [ ("F", LamAbs 1 (LamVar 1) ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))

ex3'5 :: LamMacroExpr
ex3'5 = LamDef [ ("A", LamAbs 1 (LamVar 1)), ("B", LamVar 2 ) ] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))
ex3'6 :: LamMacroExpr
ex3'6 = LamDef [] (LamAbs 2 (LamApp (LamAbs 1 (LamVar 1)) (LamVar 2)))



-- Challenge 4 --

-- Produces a lamda macro expression for a valid macro.
parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro macro | not (null macro) = Just (breakMacro macro)
                    | otherwise = Nothing

-- Breaks apart the macro into its definition and its expression.
breakMacro :: String -> LamMacroExpr
breakMacro macro | not (null i) = LamDef [([defMacro], convertExpr defExpr)] (convertExpr expr)
                 | otherwise = LamDef [] (convertExpr macro)
  where
    i = elemIndices 'i' macro
    x = head i - 1
    y = x + 4 
    def = take x macro
    defMacro = def !! 4
    defExpr = drop 8 def 
    expr = drop y macro

-- Determines what type of expression is given.
convertExpr :: String -> LamExpr
convertExpr macro | isUpper x = LamMacro cleanMacro
                  | length cleanMacro == 2 = LamVar (digitToInt y)
                  | x == 'x' = LamApp (LamVar (digitToInt y)) (convertExpr (drop 3 cleanMacro))               
                  | otherwise = LamAbs (digitToInt z) (convertExpr (drop 7 cleanMacro))
  where
    cleanMacro = removeBrackets macro
    x = head cleanMacro
    y = cleanMacro !! 1
    z = cleanMacro !! 2

-- Removes the brackets from an expression.
removeBrackets :: [Char] -> [Char]
removeBrackets macro | head macro == '(' && last macro == ')' = tail $ init macro
                     | otherwise = macro

-- These parts are experimental to read left associative rules. 

-- Splits up a macro into its function parts.
splitMacro :: String -> String
splitMacro macro | null absSplit = "(" ++ associateMacro macro ++ ")"
                 | null expressions = "(" ++ take 7 abstraction ++ splitMacro (drop 7 abstraction) ++ ")"
                 | otherwise = "(" ++ associateMacro expressions ++ ")" ++ "(" ++ take 7 abstraction ++ splitMacro (drop 7 abstraction) ++ ")"
  where
    absSplit = elemIndices '\\' macro
    expressions = take (head absSplit) macro
    abstraction = drop (head absSplit) macro

-- Breaks up the function parts by left associativity.
associateMacro :: [Char] -> [Char]
associateMacro macro = leftBrackets ++ midBrackets
  where
    leftBrackets = intercalate "" [ "(" | a <- [2..(length $ words macro)]]
    midBrackets = intercalate ")" (words macro)

-- Challenge 5

cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform _ = LamDef [] (LamVar 0)

-- Examples in the instructions
exId :: LamExpr
exId =  (LamAbs 1 (LamVar 1))
ex5'1 :: LamExpr
ex5'1 = (LamApp (LamVar 1) (LamVar 2))
ex5'2 :: LamMacroExpr
ex5'2 = (LamDef [ ("F", exId) ] (LamVar 2) )
ex5'3 :: LamMacroExpr
ex5'3 = (LamDef [ ("F", exId) ] (LamMacro "F") )
ex5'4 :: LamMacroExpr
ex5'4 = (LamDef [ ("F", exId) ] (LamApp (LamMacro "F") (LamMacro "F")))



-- Challenge 6

innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 _ = Nothing

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 _ = Nothing

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter _ _ = (Nothing,Nothing,Nothing,Nothing) 



-- Examples in the instructions

-- (\x1 -> x1 x2)
ex6'1 :: LamMacroExpr
ex6'1 = LamDef [] (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))

--  def F = \x1 -> x1 in F  
ex6'2 :: LamMacroExpr
ex6'2 = LamDef [ ("F",exId) ] (LamMacro "F")

--  (\x1 -> x1) (\x2 -> x2)   
ex6'3 :: LamMacroExpr
ex6'3 = LamDef [] ( LamApp exId (LamAbs 2 (LamVar 2)))

--  (\x1 -> x1 x1)(\x1 -> x1 x1)  
wExp :: LamExpr
wExp = (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))
ex6'4 :: LamMacroExpr
ex6'4 = LamDef [] (LamApp wExp wExp)

--  def ID = \x1 -> x1 in def FST = (\x1 -> λx2 -> x1) in FST x3 (ID x4) 
ex6'5 :: LamMacroExpr
ex6'5 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))

--  def FST = (\x1 -> λx2 -> x1) in FST x3 ((\x1 ->x1) x4))   
ex6'6 :: LamMacroExpr
ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (exId) (LamVar 4)))

-- def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1 x1) (\x1 -> x1 x1)) ID
ex6'7 :: LamMacroExpr
ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") ) 