{-# LANGUAGE TemplateHaskell #-}

module Type where

import Common

import qualified Data.Set as S
import Control.Lens

-- Types

data Effect = AEGet Name -- get(ρ)
            | AEPut Name -- put(ρ)
            | AEVar Name -- effect variable ε
            deriving (Show, Eq)

type Effects = S.Set Effect

emptyEffects :: Effects
emptyEffects = S.empty

data Type = TInt
          | TArrow DecoratedType (Name, Effects) DecoratedType -- μ---ε.φ-->μ
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

data CanonType = CanonType {
  _pvars   :: [Name]
, _tvars   :: [Name]
, _evars   :: [Name]
, _innerty :: Type
} deriving (Show, Eq)

makeLenses ''CanonType

class Canonicalizable ty where
    toCanonType   :: ty -> CanonType
    fromCanonType :: CanonType -> ty

instance Canonicalizable SimpleTS where
    toCanonType (STy ty) = CanonType [] [] [] ty
    toCanonType (SForallTy t sts)  = tvars %~ (t :) $ toCanonType sts
    toCanonType (SForallEff e sts) = evars %~ (e :) $ toCanonType sts

    fromCanonType (CanonType [] ts es ty) = foldr SForallTy (foldr SForallEff (STy ty) es) ts
    fromCanonType c = error $ "can't transform" ++ show c ++ "into SimpleTS"

