-- AST for source language

module Source where

data Expr = EVar  Name
          | EVarF Name
          | EAbs Name Expr
          | EApp Expr Expr
          | ELet Name Expr Expr
          | ELetrec Name Name Expr Expr
          deriving (Show, Eq)

type Name = String
