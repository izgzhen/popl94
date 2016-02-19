module Re.Source where

import Re.Common

data Expr = EVar Name [Type]
          | EAbs Name Type Expr
          | EApp Expr Expr
          | ELetrec Name TS Name Expr Expr
          deriving (Show, Eq)

data Type = TInt
          | TVar Name
          | TArrow Type Type
          deriving (Show, Eq)

data TS = TSTy Type
        | TSForall Name Type
        deriving (Show, Eq)


