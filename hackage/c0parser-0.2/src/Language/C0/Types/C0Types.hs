module Language.C0.Types.C0Types where

newtype Program = P Block deriving (Eq, Show)
data    Block   = B VariableDeclaration StatementSequence deriving (Eq, Show)
newtype VariableDeclaration = V [Ident] deriving (Eq, Show)
newtype StatementSequence = S [Statement] deriving (Eq, Show)
data    Statement = SS Ident --scanf
                  | SP Ident --printf
                  | SA Assignment
                  | SI IfStatement
                  | SW WhileStatement
                  | SSS StatementSequence
        deriving (Eq, Show)
data    Assignment = A Ident SimpleExpression deriving (Eq, Show)
data    IfStatement = I BoolExpression Statement (Maybe Statement) deriving (Eq, Show)
data    WhileStatement = W BoolExpression Statement deriving (Eq, Show)
data    BoolExpression = Bool SimpleExpression Relation SimpleExpression deriving (Eq, Show)
data    Relation = EQ | NE | LT | GT | LE | GE deriving (Eq, Show)
data    SimpleExpression = Simple Term [(OpAddSub, Term)] deriving (Eq, Show)
data    OpAddSub = Add | Sub deriving (Eq, Show)
data    OpMulDivMod = Mul | Div | Mod deriving (Eq, Show)
data    Term    = T Factor [(OpMulDivMod, Factor)] deriving (Eq, Show)
data    Factor  = FI Ident 
                | FN Number 
                | FS SimpleExpression deriving (Eq, Show)
type    Ident   = String 
type    Number  = Int 
