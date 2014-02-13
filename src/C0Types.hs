module C0Types where

import Control.Monad

import Test.QuickCheck

newtype Program = P Block deriving (Eq, Show)
data    Block   = B VariableDeclaration StatementSequence deriving (Eq, Show)
newtype VariableDeclaration = V [Ident] deriving (Eq, Show)
newtype StatementSequence = S [Statement] deriving (Eq, Show)
data    Statement = SS Ident -- scanf
                  | SP Ident -- printf
                  | SA Assignment
                  | SI IfStatement
                  | SW WhileStatement
                  | SSS StatementSequence
        deriving (Eq, Show)
data    Assignment = A Ident SimpleExpression deriving (Eq, Show)
data    IfStatement = I BoolExpression Statement (Maybe Statement) deriving (Eq, Show)
data    WhileStatement = W BoolExpression Statement deriving (Eq, Show)
data    BoolExpression = Bool SimpleExpression Relation SimpleExpression deriving (Eq, Show)
data    Relation = Eq | Ne | Lt | Gt | Le | Ge deriving (Eq, Show) -- CamelCase to avoid name clash
data    SimpleExpression = Simple Term [(OpAddSub, Term)] deriving (Eq, Show)
data    OpAddSub = Add | Sub deriving (Eq, Show)
data    OpMulDivMod = Mul | Div | Mod deriving (Eq, Show)
data    Term    = T Factor [(OpMulDivMod, Factor)] deriving (Eq, Show)
data    Factor  = FI Ident
                | FN Number
                | FS SimpleExpression deriving (Eq, Show)
type    Ident   = String
type    Number  = Int


class Size a where
  size :: a -> Int




--instance Arbitrary Program where
-- 	arbitrary = liftM P arbitrary

--instance Arbitrary Block where
--	arbitrary = liftM2 B arbitrary arbitrary

-- StatementSequence hängt hochgradig von der verwendeten VariableDeclaration ab
--instance Arbitrary VariableDeclaration where
--	arbitrary = liftM V arbitrary

instance Arbitrary StatementSequence where
	arbitrary = liftM S arbitrary
instance Arbitrary Statement where
	arbitrary = oneof [liftM SS arbitrary
                    ,liftM SP arbitrary
                    ,liftM SA arbitrary
                    ,liftM SI arbitrary
                    ,liftM SW arbitrary
                    ,liftM SSS arbitrary]

instance Arbitrary Assignment where
	arbitrary = liftM2 A arbitrary arbitrary

instance Arbitrary IfStatement where
	arbitrary = liftM3 I arbitrary arbitrary arbitrary
instance Arbitrary WhileStatement where
	arbitrary = liftM2 W arbitrary arbitrary
instance Arbitrary BoolExpression where
	arbitrary = liftM3 Bool arbitrary arbitrary arbitrary

instance Arbitrary Relation where
	arbitrary = elements [Eq,Ne,Lt,Gt,Le,Ge]

instance Arbitrary SimpleExpression where
	arbitrary = liftM2 Simple arbitrary arbitrary

instance Arbitrary OpAddSub where
	arbitrary = elements [Add, Sub]
instance Arbitrary OpMulDivMod where
	arbitrary = elements [Mul, Div, Mod]
instance Arbitrary Term where
	arbitrary = liftM2 T arbitrary arbitrary
instance Arbitrary Factor where
	arbitrary = oneof [liftM FI arbitrary
                    ,liftM FN arbitrary
                    ,liftM FS arbitrary]
