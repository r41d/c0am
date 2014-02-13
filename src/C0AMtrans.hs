{-# LANGUAGE TemplateHaskell #-}
module C0AMtrans where

import Prelude  hiding (EQ, NE, LT, GT, LE, GE)
import Data.Maybe

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH

import C0Types
import C0AMtypes

c0amTransQuickCheckProperties = $(testGroupGenerator)

--------------------------------------------------------------------------------
-- stupid conversion from C0Types to Operation
opaddsub2op :: OpAddSub -> Operation
opaddsub2op x = fromJust $ lookup x [(Add,ADD),(Sub,SUB)]
opmuldivmod2op :: OpMulDivMod -> Operation
opmuldivmod2op x = fromJust $ lookup x [(Mul,MUL),(Div,DIV),(Mod,MOD)]
rel2op :: Relation -> Operation
rel2op x = fromJust $ lookup x [(Eq,EQ),(Ne,NE),(Lt,LT),(Gt,GT),(Le,LE),(Ge,GE)]
-- all possible cases for x are always handled so fromJust shouldn't fail
--------------------------------------------------------------------------------
getsymtab (P b@(B vc _)) = mksymtab vc -- just for testing
--------------------------------------------------------------------------------

trans :: Program -> [Command]
trans (P b) = blocktrans b

blocktrans :: Block -> [Command]
blocktrans (B vardecl stseq) = stseqtrans stseq (mksymtab vardecl) (C 1 E)

mksymtab :: VariableDeclaration -> SymTab
mksymtab (V vars) = zip vars [1..]

stseqtrans :: StatementSequence -> SymTab -> Counter -> [Command]
stseqtrans (S [])  tab a = []
stseqtrans (S sss) tab a = foldr1 (++) $
                             map (\k@(m,n) -> sttrans m tab $ nxt a n) $
                               zip sss [1..]

sttrans :: Statement -> SymTab -> Counter -> [Command]
sttrans (SS n) tab _ = [(E, READ (fromJust $ lookup n tab))]
sttrans (SP n) tab _ = [(E, WRITE  (fromJust $ lookup n tab))]
sttrans (SA as@(A id exp)) tab _ =    simpleexptrans exp tab
                                   ++ [(E, STORE (fromJust $ lookup id tab))]
sttrans (SI ifst@(I exp stat elze)) tab a =    boolexptrans exp tab
                                            ++ [(E, JMC a)]
                                            ++ sttrans stat tab (nxt a 1)
                                            ++ case elze of
                                                    Nothing    ->    [(a, NOP)]
                                                    Just stat2 ->    [(E, JMP (nxt a 3))]
                                                                  ++ [(a, NOP)]
                                                                  ++ sttrans stat2 tab (nxt a 2)
                                                                  ++ [(nxt a 3, NOP)]
sttrans (SW whilest@(W exp stat)) tab a =    [(nxt a 2, NOP)]
                                          ++ boolexptrans exp tab
                                          ++ [(E, JMC a)]
                                          ++ sttrans stat tab (nxt a 1)
                                          ++ [(E, JMP (nxt a 2))]
                                          ++ [(a, NOP)]
sttrans (SSS stseq) tab a = stseqtrans stseq tab a

boolexptrans :: BoolExpression -> SymTab -> [Command]
boolexptrans (Bool se1 rel se2) tab =    simpleexptrans se1 tab
                                      ++ simpleexptrans se2 tab
                                      ++ [(E, rel2op rel)]

simpleexptrans :: SimpleExpression -> SymTab -> [Command]
simpleexptrans (Simple t   [])            tab = termtrans t tab
simpleexptrans (Simple t1 (x@(op,t2):xs)) tab =    termtrans t1 tab
                                                ++ termtrans t2 tab
                                                ++ [(E, opaddsub2op op)]
                                                ++ simpleexptrans' (Simple t2 xs) tab
    where
        simpleexptrans' :: SimpleExpression -> SymTab -> [Command]
        simpleexptrans' (Simple _  [])           tab = []
        simpleexptrans' (Simple _ (x@(op,t):xs)) tab =    termtrans t tab
                                                       ++ [(E, opaddsub2op op)]
                                                       ++ simpleexptrans (Simple t xs) tab

termtrans :: Term -> SymTab -> [Command]
termtrans (T f  [])             tab = factortrans f tab
termtrans (T f1 (x@(op,f2):xs)) tab =    factortrans f1 tab
                                      ++ factortrans f2 tab
                                      ++ [(E, opmuldivmod2op op)]
                                      ++ termtrans' (T f2 xs) tab
          where termtrans' :: Term -> SymTab -> [Command]
                termtrans' (T _  [])           tab = []
                termtrans' (T _ (x@(op,f):xs)) tab =    factortrans f tab
                                                     ++ [(E, opmuldivmod2op op)]
                                                     ++ termtrans' (T f xs) tab

factortrans :: Factor -> SymTab -> [Command]
factortrans (FI id) tab = [(E, LOAD (fromJust $ lookup id tab))]
factortrans (FN z) tab = [(E, LIT (BracketlessInt z))]
factortrans (FS se) tab = simpleexptrans se tab
