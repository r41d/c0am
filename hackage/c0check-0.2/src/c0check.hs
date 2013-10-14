module Main where

import System.Environment (getArgs)
import Language.C0.Parser.C0Parser (parseProg)

checkFile fn = do
  input <- readFile fn
  putStr "Folgendes Programm wurde gelesen:\n\n"
  putStr input
  putStr "\n\n"
  case parseProg input of
    Left err -> error (show err)
    Right _  -> putStr "Ihr Programm ist syntaktisch korrektes C0."
  putStr "\n\n"
  

main = do
  args <- getArgs
  case args of
   []   -> error "Die zu testende Quellcodedatei ist als Argument zu uebergeben."
   [fn] -> checkFile fn
   _    -> error "Zuviele Parameter. Aufruf nur als c0check prog.c"
