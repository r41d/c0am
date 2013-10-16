module C0AM where

import Prelude  hiding (EQ, NE, LT, GT, LE, GE)
import Data.List
import System.Environment (getArgs)

import C0Types
import C0Parser as P
import C0AMtypes
import C0AMtrans
import C0AMformat

--import Debug.Trace as T (trace)
trace _ x = x

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> error "no argument"
    [file] -> do input <- readFile file
                 putStrLn $ c0am input
    file:[dbg] -> do input <- readFile file
                     if dbg == "debug"
                     then testit input
                     else putStrLn $ c0am input
    _      -> error "too many arguments"

c0am :: String -> String
c0am c = case P.parseProg c of
             Left  err  -> error (show err)
             Right prog -> prog2am prog

prog2am :: Program -> String
prog2am = finalize . linearize . adjustJumps . eraseNOPs . {- sumNOPs . -} unifyCounters . trans

-- was useful during development
testit :: String -> IO ()
testit inp = do putStrLn $ "C0 program:\n" ++ inp
                case P.parseProg inp of
                    Left err    -> error (show err)
                    Right prog  -> do
                        putStrLn "Program is correct c0!"
                        putStrLn $ "Here it is: " ++ show prog
                        putStrLn $ "Symtab: " ++ show (getsymtab prog)

                        let transed = trans prog
                        putStrLn $ "Transed: " ++ show transed

                        let thedoubles = doubles transed
                        putStrLn $ "Doubles: " ++ show thedoubles

                        let exchanged = unifyCounters transed
                        putStrLn $ "Exchanged: " ++ show exchanged

                        let collapsed = sumNOPs exchanged
                        putStrLn $ "Collapsed: " ++ show collapsed

                        let obliterated = eraseNOPs collapsed
                        putStrLn $ "Obliterated: " ++ show obliterated

                        let adjusted = adjustJumps obliterated
                        putStrLn $ "Adjusted: " ++ show adjusted
                       
                        let linearized = linearize adjusted
                        putStrLn $ "Linearized: " ++ show linearized

                        let result = finalize linearized
                        putStrLn "THE RESULT: "
                        putStrLn result
