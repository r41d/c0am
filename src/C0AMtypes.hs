{-# LANGUAGE TemplateHaskell #-}
module C0AMtypes where

import Prelude  hiding (LT, EQ, NE, GT, LE, GE)
import Data.Maybe
import Control.Monad

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH

import C0Types

c0amTypesQuickCheckProperties = $(testGroupGenerator)

data Operation = READ Int | WRITE Int
               | LOAD Int | STORE Int | LIT BracketlessInt
               | ADD | MUL | SUB | DIV | MOD
               | LT | EQ | NE | GT | LE | GE -- hide these from Prelude
               | JMP Counter | JMC Counter
               | NOP
     deriving (Show,Eq)

newtype BracketlessInt = BracketlessInt Int  deriving (Eq)

instance Show BracketlessInt where
  show (BracketlessInt x) | x >=0 = show x
                          | x < 0 = '-' : show (abs x)

type SymTab = [(Ident,Number)]

type Command = (Counter,Operation)

newtype CommandLin = CommandLin (Int,Operation)

instance Show CommandLin where
  show (CommandLin (i,c)) = show i ++ ": " ++ show c ++ ";"

-- Abbildung nichtlinearisierter Addressen
data Counter = C Int Counter  -- Continue
             | E              -- End

instance Show Counter where
  show (E) = "0"
  show (C x E) = show x
  show (C x c) = show x ++ '.' : show c

instance Eq Counter where
  E       == E        = True
  (C x c) == (C y d)  = x == y && c == d
  _       == _        = False

nxt :: Counter -> Int -> Counter
nxt (C x E) n = C x (C n E)
nxt (C x c) n = C x (nxt c n)
nxt E       _ = E

