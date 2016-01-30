module Type where

import Common

import qualified Data.Set as S

-- Types

data Effect = AEGet Name -- get(ρ)
            | AEPut Name -- put(ρ)
            | AEVar Name -- effect variable ε
            deriving (Show, Eq)

emptyEffects :: S.Set Effect
emptyEffects = S.empty

data Type = TInt
          | TArrow DecoratedType (Name, S.Set Effect) DecoratedType -- μ---ε.φ-->μ
          | TVar Name -- α
          deriving (Show, Eq)

data Place = PVar Name -- region variables
           | PReg Name -- region names
           deriving (Show, Eq)

type DecoratedType = (Type, Place)

-- For non-recursive
data SimpleTS = STy Type
              | SForallTy  Name SimpleTS
              | SForallEff Name SimpleTS
              deriving (Show, Eq)

-- For recursive
data CompoundTS = CTy Type
                | CForallTy  Name CompoundTS
                | CForallEff Name CompoundTS
                | CForallPlc Name CompoundTS
                deriving (Show, Eq)
