-- AST for target language

module Target where

import Common
import Type

data Expr = EVar Name
          | EAbs Name Expr Place
          | EApp Expr Expr
          | ELet Name Expr Expr
          | ELetrec Name [Name] Name Place Expr Expr
          | EAt  Name [Place] Place
          | ELetReg Name Expr
          deriving (Show, Eq)
