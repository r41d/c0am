module Language.C0.Parser.C0Parser where

import Prelude hiding (Ordering(..))
import Language.C0.Types.C0Types
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Token (integer)
import Text.Parsec.String (Parser)
import Control.Monad (sequence_)
import Data.List (intersperse, nub, foldl1')

--import Debug.Trace (trace)
trace _ x = x

keywords :: [String]
keywords = [ "auto",     "break",  "case",     "char",   "const",    "continue"
           , "default",  "do",     "double",   "else",   "enum",     "extern"
           , "float",    "for",    "goto",     "if",     "int",      "long"
           , "register", "return", "short",    "signed", "sizeof",   "static"
           , "struct",   "switch", "typedef",  "union",  "unsigned", "void"
           , "volatile", "while"]

yesNo :: String -> Bool
yesNo input = 
  case runP pYesNo [] "While Parsing C0 Program ..." input of
    Right b -> b
    Left  e -> error ("The impossible happened! A never failing parser failed with: " ++ show e)

pYesNo = (try pProgram >> return True) <|> return False

wsOrCmts :: CharParser [String] ()
wsOrCmts = do
  many $ try (spaces >> (try comment1 <|> try comment2))
  spaces
  where
    comment1 = do
      string "//" 
      manyTill anyChar (char '\n')
      return ()
    comment2 = do
      string "/*"
      manyTill anyChar (string "*/")
      return ()

parseProg :: String -> (Either ParseError Program)
parseProg input = runP pProgram [] "While Parsing C0 Program ..." input

pProgram :: CharParser [Ident] Program
pProgram = do
  wsOrCmts
  string "#include"
  wsOrCmts
  char '<'
  wsOrCmts
  string "stdio.h"
  wsOrCmts
  char '>'
  wsOrCmts
  string "int"
  wsOrCmts
  string "main"
  wsOrCmts
  char '('
  wsOrCmts
  char ')'
  block <- pBlock
  return $ P block

pBlock :: CharParser [String] Block
pBlock = do
  wsOrCmts
  char '{'
  wsOrCmts
  (V decls) <- pVariableDeclaration
  setState(decls)
  (S stmts) <- pStatementSequence
  wsOrCmts
  string "return"
  wsOrCmts
  char '0'
  wsOrCmts
  char ';'
  wsOrCmts
  char '}'
  return $ B (V decls) (S stmts)

pVariableDeclaration :: CharParser [String] VariableDeclaration
pVariableDeclaration = do
  wsOrCmts
  (try (string "int")) <|> return []
  decls <- sepBy1 pIdent (wsOrCmts >> char ',')
  wsOrCmts
  char ';'
  if nub decls == decls 
    then (if (not $ foldr (||) False $ map isKeyword decls) 
        then return (V decls) 
        else fail "C-Schluesselwort als Bezeichner verwendet")
    else fail "Doppeldeklaration"

isKeyword :: String -> Bool
isKeyword ident = ident `elem` keywords

pIdent :: CharParser [String] String
pIdent = do
  trace "try to read ident ...\n" wsOrCmts
  hd <- try letter <|> char '_'
  tl <- many (try letter <|> try digit <|> char '_')
  trace ("return ident " ++ (hd:tl) ++ "\n") (return (hd:tl))

checkIdent :: String -> CharParser [String] ()
checkIdent ident = do
  idents <- trace ("DEBUG: checkIdent " ++ ident ++ "\n") getState
  if ident `elem` idents then trace "DEBUG: ... successful!\n" (return ()) else fail ("unknown identifier " ++ ident)

pStatementSequence :: CharParser [String] StatementSequence
pStatementSequence = 
    many1 (try pStatement) >>= (\stmts -> return (S stmts))

pStatement :: CharParser [String] Statement
pStatement = foldl1' (<|>) $ map try 
  [ trace "try pScanf ...\n" pScanf
  , trace "try pPrintf ...\n" pPrintf
  , trace "try pAssignment ...\n" pAssignment
  , trace "try pIfStatement ...\n" pIfStatement
  , trace "try pWhileStatement ...\n" (pWhileStatement <|> trace "DEBUG: WHILE FAILED" (fail "BLUB"))
  , trace "try pStatementSequence' ...\n" pStatementSequence'
  ]
  where pStatementSequence' = do
         wsOrCmts
         trace "DEBUG: try to read {" $ char '{' 
         wsOrCmts
         stmts <- trace "DEBUG: read statement sequence ...\n" pStatementSequence 
         wsOrCmts
         char '}'
         return (SSS stmts)

pScanf :: CharParser [String] Statement 
pScanf = do
  wsOrCmts
  string "scanf"
  wsOrCmts
  char '('
  wsOrCmts
  string "\"%d\""
  wsOrCmts
  char ','
  wsOrCmts
  char '&'
  wsOrCmts
  ident <- pIdent
  checkIdent ident
  wsOrCmts
  char ')'
  wsOrCmts
  char ';'
  return (SS ident)

pPrintf :: CharParser [String] Statement 
pPrintf = do
  wsOrCmts
  string "printf"
  wsOrCmts
  char '('
  wsOrCmts
  string "\"%d\""
  wsOrCmts
  char ','
  wsOrCmts
  ident <- pIdent
  checkIdent ident
  wsOrCmts
  char ')'
  wsOrCmts
  char ';'
  return (SP ident)

pAssignment :: CharParser [String] Statement 
pAssignment = do
  wsOrCmts
  ident <- pIdent
  wsOrCmts
  char '='
  checkIdent ident
  wsOrCmts
  sexp <- pSimpleExpression
  wsOrCmts
  char ';'
  return $ SA (A ident sexp)

pIfStatement :: CharParser [String] Statement 
pIfStatement = do
  wsOrCmts
  trace "try to read if\n" $ string "if"
  wsOrCmts
  char '('
  wsOrCmts
  bexp <- pBoolExpression
  wsOrCmts 
  char ')'
  stmt1 <- trace "parsed if part" pStatement
  wsOrCmts
  try (trace "parsed else" (string "else") >> wsOrCmts >> pStatement >>= (\stmt2 -> return $ SI (I bexp stmt1 (Just stmt2)))) <|> return (SI (I bexp stmt1 Nothing))

pWhileStatement :: CharParser [String] Statement 
pWhileStatement = (do
  wsOrCmts
  trace "DEBUG: Testing for while\n" (string "while")
  wsOrCmts
  char '('
  bexp <- trace "DEBUG: try to read boolean expression\n" pBoolExpression
  trace "DEBUG: ... successfull!\n" wsOrCmts
  char ')'
  wsOrCmts
  stmt <- trace "DEBUG: try to read whileStatement\n" pStatement
  return $ SW (W bexp stmt))
  <|> (do
         (State s _ u) <- getParserState
         trace ("DEBUG: Simple Expression failed at ParserState:\n" ++ show s ++ "\n") fail "failed"
      )

pBoolExpression :: CharParser [String] BoolExpression 
pBoolExpression = do
  sexp1 <- trace "DEBUG: try to read simpleExpression\n" pSimpleExpression
  rel <- trace "DEBUG: try to read relation\n" pRelation
  sexp2 <- trace "DEBUG: try to read second simple expression" pSimpleExpression
  return $ Bool sexp1 rel sexp2

pRelation :: CharParser [String] Relation
pRelation = try $ wsOrCmts >> (foldl1' (<|>) $ map try 
   [ string "==" >> return EQ
   , string "!=" >> return NE
   , string "<=" >> return LE
   , string ">=" >> return GE
   , char '<'    >> return LT
   , char '>'    >> return GT
   ])

pSimpleExpression :: CharParser [String] SimpleExpression
pSimpleExpression = (do
  wsOrCmts
  (State s _ u) <- getParserState
  term <- trace ("DEBUG: try to read term at parser state:\n" ++ s ++ "\nwith user state: " ++ show u ++ "\n") pTerm
  terms <- trace "DEBUG: try to read more terms\n" pMoreTerms
  return $ Simple term terms) <|> 
  (do
    (State s _ u) <- getParserState
    trace ("DEBUG: Simple Expression failed at ParserState:\n" ++ show s ++ "\n") fail "failed")
  
pTerm :: CharParser [String] Term
pTerm = (do
  trace "entered pTerm\n" (return ())
  (State s _ u) <- getParserState
  fac <- trace ("DEBUG: try to read factor at ParserState:\n" ++ s ++ "\n") pFactor
  facs <- trace ("DEBUG: try to read more factors ...") pMoreFactors
  trace ("read Term " ++ show (T fac facs) ++ "\n") return ()
  (return $ T fac facs))
         <|> trace "DEBUG: pTerm failed" fail "failed"

pMoreTerms :: CharParser [String] [(OpAddSub, Term)]
pMoreTerms = many $ try (pOpAddSub >>= (\op->pTerm >>= (\t -> return (op,t))))

pOpAddSub :: CharParser [String] OpAddSub
pOpAddSub = wsOrCmts >> ((char '+' >> return Add) <|> (char '-' >> return Sub))

pFactor :: CharParser [String] Factor
pFactor = wsOrCmts >> (foldl1' (<|>) $ map try
            [ trace "DEBUG pFactor ident? ...\n" (pIdent >>= (\ident -> (checkIdent ident >> return (FI ident))))
            , trace "DEBUG pFactor number? ...\n" (pNumber >>= (\number -> return (FN number)))
            , trace "DEBUG pFactor (simple expression)? ...\n" (char '(' >> wsOrCmts >> pSimpleExpression >>= (\sexp -> (wsOrCmts >> char ')' >> return (FS sexp))))
            ])

pMoreFactors :: CharParser [String] [(OpMulDivMod, Factor)]
pMoreFactors = many (try (do
      op <- pOpMulDivMod 
      fac <- pFactor
      return (op,fac)))

pOpMulDivMod :: CharParser [String] OpMulDivMod
pOpMulDivMod = do
  op <-trace "try to read mul div mod ops\n" $ wsOrCmts >> ((char '*' >> return Mul) <|> (char '/' >> return Div) <|> (char '%' >> return Mod))
  trace ("read: " ++ show op ++ "\n") $ return op

pNumber :: CharParser [String] Int
pNumber = do
  wsOrCmts
  sign <- char '-' <|> return ' '
  digits <- many1 digit
  if sign == '-' then return ((read (sign:digits))::Int) else return ((read digits)::Int)
