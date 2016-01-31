-- AST for source language

module Source where

import Common

data Expr = EInt Int
          | EVar Name
          | EFun Name
          | EAbs Name Expr
          | EApp Expr Expr
          | ELet Name Expr Expr
          | ELetrec Name Name Expr Expr
          deriving (Show, Eq)
