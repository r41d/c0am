{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module C0AMtypes where

import Prelude  hiding (LT, EQ, NE, GT, LE, GE)
import Data.Monoid
import Control.Monad
import Test.QuickCheck
import Data.Maybe
import C0Types

class Empty a where
  empty :: a -- some boring, 'empty' element which doesnt contain a lot of information

data Operation = READ Int | WRITE Int
               | LOAD Int | STORE Int | LIT BracketlessInt
               | ADD | MUL | SUB | DIV | MOD
               | LT | EQ | NE | GT | LE | GE -- hide these from Prelude
               | JMP Counter | JMC Counter
               | NOP
     deriving (Show,Eq)
instance Empty Operation where
  empty = NOP

instance Arbitrary Operation where
  arbitrary = oneof $ [liftM READ positive, liftM WRITE positive,
                       liftM LOAD positive, liftM STORE positive,
                       liftM LIT (liftM BracketlessInt smallint)] ++
                      [ return x | x <- [EQ,NE,LT,GT,LE,GE] ] ++
                      [liftM JMP arbitrary, liftM JMC arbitrary] ++
                      [return NOP]
positive :: Gen Int
positive = choose (0,999)
smallint :: Gen Int
smallint = choose ((-999),999)

-- cheat to get rid of the () for negative ints
newtype BracketlessInt = BracketlessInt Int  deriving (Eq)
instance Show BracketlessInt where
  show (BracketlessInt x) | x >=0 = show x
                          | x < 0 = '-' : show (abs x)

type SymTab = [(Ident,Int)]
instance Empty SymTab where
  empty = []

-- Command = Counter + Operation
type Command = (Counter,Operation)
instance Empty Command where
  empty = (E,NOP)

-- linearized commands
newtype CommandLin = CommandLin (Int,Operation)
instance Show CommandLin where
  show (CommandLin (i,c)) = show i ++ ": " ++ show c ++ ";"

-- Abbildung nichtlinearisierter Addressen
--             Continue        End
data Counter = C Int Counter | E
instance Empty Counter where
  empty = E
instance Arbitrary Counter where
  arbitrary = sized countergen
    where countergen :: Int -> Gen Counter
          countergen n | n == 0 = return E
                       | n > 10 = countergen (n`mod`5)
                       | True   = liftM2 C (choose (0,n)) (countergen (n-1))
instance Show Counter where
  show (E) = "0"
  show (C x E) = show x
  show (C x c) = show x ++ '.' : show c
instance Eq Counter where
  E       == E        = True
  (C x E) == (C y E)  = x == y
  (C x c) == (C y d)  = x == y && c == d
  _       == _        = False
nxt :: Counter -> Int -> Counter
nxt (C x E) n = C x (C n E)
nxt (C x c) n = C x (nxt c n)
nxt E       _ = E
