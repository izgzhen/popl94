-- AST for target language

module Target where

type Name = String

data Place = PVar Name
           | PReg Name

data Expr = EVar Name
          | EAbs Name Expr Place
          | EApp Expr Expr
          | ELet Name Expr Expr
          | ELetrec Name [Name] Name Place Expr Expr
          | EPApp Name [Place] Place
          | ELetReg Name Expr
          deriving (Show, Eq)
