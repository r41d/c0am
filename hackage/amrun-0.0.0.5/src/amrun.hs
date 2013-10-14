module Main where

import Prelude hiding (Ordering(..))
import System.Environment (getArgs)
import System.IO
import System.IO.Unsafe
import Control.DeepSeq
import Data.Char (toUpper)

import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.String (Parser)
--import Debug.Trace
trace _ x = x

data Command = READ Int | WRITE Int | LOAD Int 
             | STORE Int | LIT Int | ADD | MUL 
             | SUB | DIV | MOD | LT | EQ | NE 
             | GT | LE | GE | JMP Int | JMC Int 
     deriving (Show)


step :: Command -> (Int,[Int],[(Int,Int)],[Int],[Int])
        -> (Int,[Int],[(Int,Int)],[Int],[Int])
step (READ n) (m,d,h,first:rest,out) = 
   (m+1,d,update h n first,rest,out)
step (WRITE n) (m,d,h,inp,out) = 
   (m+1,d,h,inp,out ++ [write (get m h n)])
step (LOAD n) (m,d,h,inp,out) = 
   let d1 = get m h n in seq d1 (m+1,d1:d,h,inp,out)
step (STORE n) (m,d1:d',h,inp,out) = 
   (m+1,d',update h n d1,inp,out)   
step (STORE _) (m,[],_,_,_) = 
   error $ "In Programmzeile " ++ show m ++ ", Versuch auf leeren Datenkeller zuzugreifen."
step (LIT z) (m,d,h,inp,out) = (m+1,z:d,h,inp,out)
step ADD (m,d1:d2:d',h,inp,out) = 
   (m+1,(d2+d1):d',h,inp,out)
step MUL (m,d1:d2:d',h,inp,out) = 
   (m+1,(d2*d1):d',h,inp,out)
step SUB (m,d1:d2:d',h,inp,out) = 
   (m+1,(d2-d1):d',h,inp,out)
step DIV (m,d1:d2:d',h,inp,out) = 
   (m+1,(div d2 d1):d',h,inp,out)
step MOD (m,d1:d2:d',h,inp,out) = 
   (m+1,(mod d2 d1):d',h,inp,out)
step LT (m,d1:d2:d',h,inp,out) = 
   (m+1,(if d2<d1 then 1 else 0):d',h,inp,out)
step EQ (m,d1:d2:d',h,inp,out) = 
   (m+1,(if d2==d1 then 1 else 0):d',h,inp,out)
step NE (m,d1:d2:d',h,inp,out) = 
   (m+1,(if d2/=d1 then 1 else 0):d',h,inp,out)
step GT (m,d1:d2:d',h,inp,out) = 
   (m+1,(if d2>d1 then 1 else 0):d',h,inp,out)
step LE (m,d1:d2:d',h,inp,out) = 
   (m+1,(if d2<=d1 then 1 else 0):d',h,inp,out)
step GE (m,d1:d2:d',h,inp,out) = 
   (m+1,(if d2>=d1 then 1 else 0):d',h,inp,out)
step (JMP e) (m,d,h,inp,out) = (e,d,h,inp,out)
step (JMC e) (m,0:d',h,inp,out) = (e,d',h,inp,out)
step (JMC e) (m,1:d',h,inp,out) = (m+1,d',h,inp,out)
step (JMC _) (m,_:_,_,_,_) = error $ "In Programmzeile " ++ show m ++ ", versuchter JMC, obwohl Datenkellerspitze weder 0 noch 1."
step _ (m,[],_,_,_) = 
   error $ "In Programmzeile " ++ show m ++ ", Versuch auf leeren Datenkeller zuzugreifen."
step _ (m,[_],_,_,_) = 
   error $ "In Programmzeile " ++ show m ++ ", nicht genug Werte auf Datenkeller."

get :: Int -> [(Int,Int)] -> Int -> Int
get m ((a,b):h) n = if a==n then b else get m h n
get m [] _ = error $ "In Programmzeile " ++ show m ++ ", Versuch auf unbelegten Hauptspeicherplatz zuzugreifen."

update :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
update ((a,b):h) n c | a==n = (a,c):h
                     | a<n  = (a,b):(update h n c)
                     | a>n  = (n,c):(a,b):h
update []        n c = [(n,c)]


run :: [Command] -> (Int,[Int],[(Int,Int)],[Int],[Int]) 
       -> [(Int,[Int],[(Int,Int)],[Int],[Int])]
run prog conf = if valid prog conf then conf:(run prog (next prog conf)) 
                                   else [conf]

valid :: [Command] -> (Int,[Int],[(Int,Int)],[Int],[Int]) -> Bool
valid prog (m,d,h,inp,out) = 1<=m && m<=(length prog)

next :: [Command] -> (Int,[Int],[(Int,Int)],[Int],[Int]) 
        -> (Int,[Int],[(Int,Int)],[Int],[Int])
next prog (m,d,h,inp,out) = step (prog !! (m-1)) 
                                 (m,d,h,inp,out)


--- BEGIN PARSER

parseProgFromFile :: String -> IO (Either ParseError [Command])
parseProgFromFile fname = do
    input <- readFile fname
    return (runP progParser 1 fname input)

progParser :: CharParser Int [Command]
progParser = do
    ret <- many (try (lineParser <?> ""))
    spaces <?> ""
    (State s _ _) <- getParserState
    trace ("Parser State:\n" ++ s) (return ret)
    if s == "" 
     then return (fromJust ret)
     else fail ("\n\nthe following program part can't be parsed:\n\n" ++ s)
  where fromJust [] = []
        fromJust (Nothing : mas) = fromJust mas
        fromJust ((Just a) : mas) = a : fromJust mas

lineParser :: CharParser Int (Maybe Command)
lineParser = 
  (try commentParser >> return Nothing)
  <|> (do {
     pc <- pcParser <?> "expected command number\n";
     ppc <- getState;
     if pc /= ppc
     then ((fail ("expected command number " ++ show ppc ++ ", but read command number " ++ show pc ++ "\n")))
     else do {
      char ':' <?> "expected ':'";
      spaces <?> "";
      cmd <- (commandParser <?> "");
      spaces <?> "";
      char ';' <?> "missing ';'";
      spaces <?> "";
      (commentParser <|> return ()) <?> "comment or end of line or other command number";
      modifyState (+1);
      return (Just cmd);
         }
     })

pcParser :: CharParser Int Int
pcParser = do
    spaces <?> ""
    num <- (many1 (digit <?> "")) <?> ""
    spaces <?> ""
    return ((read num)::Int)

commandParser :: CharParser Int Command
commandParser = do {
 cmd <- ((many1 letter) <|> lookAhead ((space <|> char ';') >> return "")) <?> "expected command";
 spaces <?> "";
 let noArgCmd = readCmd (map toUpper cmd)
     oneArgCmd = readCmd' (map toUpper cmd)
 in
 case noArgCmd of
   Just c -> return c
   Nothing -> 
     case oneArgCmd of
       Just c -> do
                   sign <- if cmd == "LIT" then neg else return 1
                   arg <- (pcParser <?> "command argument")
                   if cmd == "LIT"  || cmd == "JMC" || cmd == "JMP"
                     then return (c (sign*arg))
                     else (if arg == 0 then unexpected (cmd ++ " does not take 0 as argument.") else return (c arg))
       Nothing -> unexpected (cmd ++ " is not a command.")
                   }
  where
    neg = spaces >> (try (char '-' >> ((many1 space >> fail "spaces after '-', expected digit") <|> return (-1))) <|> return 1)
    readCmd cmd = case cmd of
                   "ADD" -> Just ADD
                   "MUL" -> Just MUL
                   "SUB" -> Just SUB
                   "DIV" -> Just DIV 
                   "MOD" -> Just MOD 
                   "LT"  -> Just LT 
                   "EQ"  -> Just EQ
                   "NE"  -> Just NE 
                   "GT"  -> Just GT
                   "LE"  -> Just LE
                   "GE"  -> Just GE
                   _     -> Nothing
    readCmd' cmd = case cmd of
                    "READ"  -> Just READ
                    "WRITE" -> Just WRITE
                    "LOAD"  -> Just LOAD
                    "STORE" -> Just STORE
                    "LIT"   -> Just LIT
                    "JMP"   -> Just JMP
                    "JMC"   -> Just JMC 
                    _       -> Nothing
                  


commentParser :: CharParser Int ()
commentParser = do {
    spaces >> try ((char '#') <?> "");
--    (State s _ _) <- getParserState;
--    trace ("Parser State comment1:\n" ++ s) (return ());
    manyTill anyChar (lookAhead (char '\n'));
--    (State s _ _) <- getParserState;
--    trace ("Parser State comment2:\n" ++ s) (return ()); 
    return ();
    }
--- END PARSER

printProg :: String -> Int -> [Command] -> IO()
printProg fn cols cs = writeFile fn $ showProg cols cs


showProg :: Int -> [Command] -> String
showProg cols commands = let rows = ((length(commands)-1) `div` cols)+1 in trace ("maxrows = " ++ show rows) $ showProg' 0 0 (length commands) commands cols rows

showProg' :: Int -> Int -> Int -> [Command] -> Int -> Int -> String
showProg'    row    col    clen   c            maxcol maxrow =
    let pc = (col * maxrow) + row in
    if pc >= clen
     then "\n" ++ (if row < maxrow-1 then showProg' (row+1) 0 clen c maxcol maxrow else "")
     else
      (if pc<9 then " " else "") ++
      show (pc+1) ++ ":  " ++ show (c!!pc) ++ ";" ++
      (if col < maxcol-1 then "\t" ++ showProg' row (col+1) clen c maxcol maxrow 
       else "\n" ++ (if row < maxrow-1 then showProg' (row+1) 0 clen c maxcol maxrow else ""))

initial :: (Int,[Int],[(Int,Int)],[Int],[Int])
initial = (1,[],[],map inp [0..],[])
          where inp x = (read (unsafePerformIO (putStr (if x>0 then "naechste Eingabe: " else "naechste Eingabe: ") >> hFlush stdout >> getLine)))::Int

write :: Int -> Int
write n = unsafePerformIO (putStr "naechste Ausgabe: " >> print n >> return n)

main = do 
          args <- getArgs
          case args of
            []      -> error "Bitte uebergeben Sie die auszufuehrende Datei als Argument (Bsp. ./amrun prog.am)"
            [fname] -> do
                          parsed <- parseProgFromFile fname
                          case parsed of
                            Left err -> putStrLn $ show err
                            Right prog -> do
                                            putStrLn ("Es laeuft das Programm:\n\n" ++ showProg 3 prog) 
                                            seq (last (force (run prog initial))) (return ())
            (_:_)        -> error "Mehrere Argumente uebergeben. Uebergeben Sie nur die auszufuehrende Datei als Argument (Bsp. ./amrun prog.am)"
  where force [] = []
        force (e@(_,_,hs,_,out):rest) = deepseq hs (deepseq out (e:force rest))
