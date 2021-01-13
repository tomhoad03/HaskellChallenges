{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2020
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges ( WordSearchGrid,Placement,Posn,Orientation(..),solveWordSearch, createWordSearch,
    LamMacroExpr(..),LamExpr(..),prettyPrint, parseLamMacro,
    cpsTransform,innerRedn1,outerRedn1,compareInnerOuter ) where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
-- We import System.Random - make sure that your installation has it installed - use stack ghci and stack ghc
import Data.Char ( digitToInt, isUpper )
import Parsing ()
import Control.Monad ()
import Data.List
    ( elemIndices,
      findIndex,
      findIndices,
      group,
      groupBy,
      isPrefixOf,
      sort,
      sortBy,
      tails )
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq ( NFData )
import System.IO ()
import System.Random ( getStdRandom, Random(randomR) )

-- My import statements.
import Data.Function ( on )
import Data.Maybe ( fromMaybe, isJust, isNothing )

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

-- END OF CODE YOU MUST NOT MODIFY; ADD YOUR OWN CODE HERE

-- Challenge 1 --
-- Solves a given word search from a list of given words
solveWordSearch :: [String] -> WordSearchGrid -> [(String, Maybe Placement)]
solveWordSearch _ [] = error "No word search grid given."
solveWordSearch [] _ = []
solveWordSearch words grid = filteredCheckedWords ++ missingWords
  where
    -- Checks every word in every position.
    gridSize = length grid
    checkedWords = [findWords words grid (x, y) | y <- [0..gridSize - 1], x <- [0..gridSize - 1]]
    filteredCheckedWords = filter (isJust . snd) checkedWords
    missingWords = [(a, Nothing) | a <- filter (`notElem` map fst filteredCheckedWords) words]

-- Checks every word at a given position.
findWords :: [String] -> WordSearchGrid -> Posn -> (String, Maybe Placement)
findWords words grid (x, y) | null words = ("", Nothing)
                            | isNothing (snd foundWord) = findWords (tail words) grid (x, y)
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
matchWords word = filter (\x -> fst x == word)

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



-- Challenge 2 --
-- Produces a solvable word search grid
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
                               return (map head (groupBy (\x y -> fst x == fst y) (sortBy (compare `on` fst) (wordGrid ++ concat randGrid))))
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

--- Challenge 1 and 2 development testing functions
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



-- Challenge 3 --
-- Pretty prints a lamda macro expression
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

-- Prints macro and variable expressions.
readExpr :: LamExpr -> [(String, LamExpr)] -> String
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
-- Currently printing double backslashes in abstraction expressions - must read up on escaping this.

-- Challenge 3 development testing functions.
testGetValue :: String
testGetValue = getValue (LamAbs 1 (LamVar 1)) [("F", LamAbs 1 (LamVar 1))]

testCheckDefs :: Bool
testCheckDefs = checkDefs (LamAbs 1 (LamVar 1)) [("F", LamAbs 1 (LamVar 1))]



-- Challenge 4 --
-- Produces a lamda macro expression for a valid macro string
parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro macro | null macro = Nothing
                    | length (elemIndices 'd' macro) > 1 = Nothing -- Repeated definitions
                    | length macro < 16 && 'd' `elem` macro = Nothing -- Macro body not closed
                    | otherwise = Just (breakMacro macro) -- Acceptable input

-- Breaks apart the macro into its definition and its expression.
breakMacro :: String -> LamMacroExpr
breakMacro macro | not (null i) = LamDef [([defMacro], convertExpr defExpr)] (convertExpr expr) -- With a definition
                 | otherwise = LamDef [] (convertExpr macro) -- Without a definition
  where
    i = elemIndices 'i' macro
    x = head i - 1
    y = x + 4 
    def = take x macro
    defMacro = def !! 4
    defExpr = drop 8 def 
    expr = drop y macro

-- Go between function that reads the explicit brackets before it converts.
convertExpr :: String -> LamExpr
convertExpr macro = uncurry findAbs macrodMacro
  where
    macrodMacro = replaceBrackets macro

-- Finds an abstraction in macro.
findAbs :: String -> [(String, Char)] -> LamExpr
findAbs macro macrodMacros | not (null absLoc) && head absLoc > 0 = LamApp (readMacro (take (head absLoc - 1) macro) macrodMacros) lamExpr -- Application on an abstraction 
                           | not (null absLoc) && head absLoc == 0 = lamExpr -- Just abstraction
                           | otherwise = readMacro macro macrodMacros -- No abstraction
  where
    absLoc = elemIndices '\\' macro
    lamExpr = LamAbs (digitToInt (macro !! (head absLoc + 2))) (findAbs (drop (head absLoc + 7) macro) macrodMacros)

-- Reads a macro and writes the equivilant lamda expression.
readMacro :: String -> [(String, Char)] -> LamExpr
readMacro macro macrodMacros | length macro > 2 = LamApp (readMacro (take space macro) macrodMacros) (readMacro (drop (space+1) macro) macrodMacros) -- Application
                             | length macro == 2 = LamVar (digitToInt (last macro)) -- Lamda variable
                             | length macro == 1 = checkMacros macro macrodMacros -- Lambda macro
                             | otherwise = LamVar (-1) -- Fail state
  where
    space = last $ elemIndices ' ' macro

-- Check lamda macros if they are part of the definition, if not, they are converted back to their expressions.
checkMacros :: [Char] -> [(String, Char)]-> LamExpr
checkMacros macro macrodMacros | head macro `notElem` tempMacros = LamMacro macro
                               | otherwise = convertExpr tempMacro
  where
    tempMacros = map snd macrodMacros
    tempMacro = fst $ head $ filter (\(x,y) -> y == head macro) macrodMacros

-- Replaces the explicit brackets with a temporary macro to not confuse the application rules.
replaceBrackets :: String -> (String, [(String, Char)])
replaceBrackets macro = go macro macrodBrackets
  where
    macrodBrackets = macroBrackets macro
    go string macros | not (null macros) = go removedString (tail macros)
                     | otherwise = (string, macrodBrackets)
                       where
                         toFind = fst $ head macros
                         toReplace = snd $ head macros
                         index = fromMaybe (-1) $ (toFind `isPrefixOf`) `findIndex` tails string
                         removedString = take (index - 1) string ++ [toReplace] ++ drop (index + length toFind + 1) string

-- Pairs up a temporary macro for each of the explicit brackets.
macroBrackets :: String -> [(String, Char)]
macroBrackets macro = macrodStrings
  where
    macros = filter (`notElem` findMacros macro) ['A'..'Z']
    brackets = findBrackets macro
    bracketedStrings = [ a | (x, y) <- brackets, a <- [drop (x+1) (take y macro)] ]
    macrodStrings = zip bracketedStrings macros

-- Finds any macros in the macro string. Any temporary macros won't mix with ones given in the definition.
findMacros :: String -> String
findMacros macro = [ a | (a, b) <- filter (\(x,y) -> y `elem` indices) zippedIndices ]
  where
    indices = findIndices isUpper macro
    zippedIndices = zip macro [0..(length macro - 1)]

-- Finds all the explicit brackets in the macro.
findBrackets :: String -> [(Int, Int)]
findBrackets macro | not (null brackets) = findOuterBrackets brackets
                   | otherwise = []
  where
    fstBrackets = elemIndices '(' macro
    sndBrackets = elemIndices ')' macro
    brackets = zip fstBrackets sndBrackets

-- Find the outer brackets of the macro. ( _ ( _ )) -> ( ___ )
findOuterBrackets :: Ord b => [(b, b)] -> [(b, b)]
findOuterBrackets brackets | not (null outers) = (fst first, snd final) : findOuterBrackets outers
                           | otherwise = [(fst first, snd $ last brackets)]
  where
    first = head brackets
    limit = snd first
    outers = filter (\(x,y) -> x > limit) brackets
    final = brackets !! (head (elemIndices (head outers) brackets) - 1)



-- Challenge 5
-- Converts CPS lamda calclus to a standard lamda expressions
cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform (LamDef [] expr) = LamDef [] (transformExpr 1 expr)
cpsTransform (LamDef defs expr) = LamDef (zip (map fst defs) (map (transformExpr 1 . snd) defs)) (transformExpr 1 expr)

-- Transforms a lamda expression based on what type of expression it is.
transformExpr :: Int -> LamExpr -> LamExpr
transformExpr count expr | findExpr expr == "app" = transformApp (count + 1) expr 
                         | findExpr expr == "abs" = transformAbs count expr
                         | findExpr expr == "var" = transformVar count expr
                         | findExpr expr == "macro" = expr

-- Determines what kind of lamda expression it is transforming.
findExpr :: LamExpr -> String
findExpr (LamApp _ _) = "app"
findExpr (LamAbs _ _) = "abs"
findExpr (LamVar _) = "var"
findExpr (LamMacro _) = "macro"

-- Transforms a lamda application.
transformApp :: Int -> LamExpr -> LamExpr
transformApp count (LamApp expr1 expr2) = LamAbs count (LamApp (transformExpr (count + 3) expr1) (LamAbs (count + 1)
                                            (LamApp (transformExpr (count + 4) expr2) (LamAbs (count + 2)
                                              (LamApp (LamApp (LamVar (count + 1)) (LamVar (count + 2))) (LamVar count))))))

-- Transforms a lamda abstraction.
transformAbs :: Int -> LamExpr -> LamExpr
transformAbs count (LamAbs n expr) = LamAbs count (LamApp (LamVar count) (LamAbs n (transformExpr (count + 1) expr)))

-- Transforms a lamda variable.
transformVar :: Int -> LamExpr -> LamExpr
transformVar count (LamVar n) = LamAbs count (LamApp (LamVar count) (LamVar n))



-- Challenge 6
-- Counting and comparing direct lamda calculus reductions and CPS
innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 _ = Nothing

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 _ = Nothing

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter _ _ = (Nothing,Nothing,Nothing,Nothing) 

-- Challenge 6 testing expressions
exId :: LamExpr
exId = LamAbs 1 (LamVar 1)
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
wExp = LamAbs 1 (LamApp (LamVar 1) (LamVar 1))
ex6'4 :: LamMacroExpr
ex6'4 = LamDef [] (LamApp wExp wExp)
--  def ID = \x1 -> x1 in def FST = (\x1 -> λx2 -> x1) in FST x3 (ID x4) 
ex6'5 :: LamMacroExpr
ex6'5 = LamDef [ ("ID",exId) , ("FST",LamAbs 1 (LamAbs 2 (LamVar 1))) ] ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp (LamMacro "ID") (LamVar 4)))
--  def FST = (\x1 -> λx2 -> x1) in FST x3 ((\x1 ->x1) x4))   
ex6'6 :: LamMacroExpr
ex6'6 = LamDef [ ("FST", LamAbs 1 (LamAbs 2 (LamVar 1)) ) ]  ( LamApp (LamApp (LamMacro "FST") (LamVar 3)) (LamApp exId (LamVar 4)))
-- def ID = \x1 -> x1 in def SND = (\x1 -> λx2 -> x2) in SND ((\x1 -> x1 x1) (\x1 -> x1 x1)) ID
ex6'7 :: LamMacroExpr
ex6'7 = LamDef [ ("ID",exId) , ("SND",LamAbs 1 (LamAbs 2 (LamVar 2))) ]  (LamApp (LamApp (LamMacro "SND") (LamApp wExp wExp) ) (LamMacro "ID") ) 