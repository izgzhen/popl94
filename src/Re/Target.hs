module Re.Target where

import Re.Common
import qualified Re.Source as S

import Data.Set (Set)

newtype RegVar = RegVar Name deriving (Eq, Ord, Show)

newtype EffVar = EffVar Name deriving (Eq, Ord, Show)

type AtEff = Either RegVar EffVar

type Effect = Set AtEff

type ArrEff = (EffVar, Effect)

data Type = TInt
          | TVar Name
          | TArrow AnnTy ArrEff AnnTy
          deriving (Show, Eq)

type AnnTy = (Type, RegVar)



-- Target Triple
data Term = Term Expr AnnTy Effect deriving (Show, Eq)

data Expr = EVar Name
          | EFun InstList RegVar
          | EAbs Name AnnTy Term
          | EApp Term Term
          | ELetrec Name (S.Type, RegVar) Name Term Term
          | ELetreg Basis Term
          deriving (Show, Eq)

type Basis = (Set RegVar, Set ArrEff)


data InstList = InstList [Type] [RegVar] [ArrEff] deriving (Show, Eq)

