-- AST for target language

module Target where

import Common
import Type hiding (EVar)

data Expr = EInt Int Place
          | EVar Name
          | EAbs Name Expr Place
          | EApp Expr Expr
          | ELet Name Expr Expr
          | ELetrec Name [Name] Name Place Expr Expr
          | EFun Name [Place] Place
          | ELetReg Name Expr
          deriving (Eq)

instance Show Expr where
    show (EInt i p)     = show i ++ " at " ++ show p
    show (EVar x)       = x
    show (EAbs x e p)   = "(λ" ++ x ++ "." ++ show e ++ " at " ++ show p ++ ")"
    show (EApp e1 e2)   = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (ELet x e1 e2) = "(let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
    show (ELetrec f ps x p e1 e2) =
        "(letrec " ++ f ++ "[" ++ splitByComma (map show ps) ++ "]" ++ "(" ++ x ++ ")" ++
            " at " ++ show p ++ " = " ++ show e1 ++ " in " ++ show e2 ++ ")"
    show (EFun f ps p)  = f ++ "[" ++ splitByComma (map show ps) ++ "]" ++ " at " ++ show p
    show (ELetReg x e)  = "letregion " ++ x ++ " in " ++ show e


splitByComma :: [String] -> String
splitByComma []     = ""
splitByComma [x]    = x
splitByComma (x:xs) = x ++ ", " ++ splitByComma xs
