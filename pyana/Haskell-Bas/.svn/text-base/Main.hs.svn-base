module Main where
import TI.PyData
import TI.PyEval
import TI.HskDataTranslator
import Data.SExp
import Data.Maybe
import Text.Printf
import System.Environment (getArgs)
import qualified Data.List as L

parseInput :: String -> IO MonoTypeStore
parseInput filePath = do
  contents <- readFile filePath
  let (Right sexpContents) = parseSExp contents
      hsData = sexpToProgram sexpContents    
      monoTyStore =  inferPyType  hsData
  -- putStr $ show (sexpToProgram sexpContents)                     
  return monoTyStore

genHskData :: String -> IO ()
genHskData path = do
  contents <- readFile path
  let (Right sexpContents) = parseSExp contents
      hskData = sexpToProgram sexpContents
  writeFile (path ++ ".hskData") (show hskData)


main = do
  args <- getArgs  
  res <- parseInput (L.head args)
  --print res
  genHskData  (L.head args)
  writeFile ((L.head args) ++ ".typeinfo") (show res)
  
 	

testOne path = do
   res <- parseInput path
   print (show res)
 