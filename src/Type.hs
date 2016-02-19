{-# LANGUAGE TemplateHaskell #-}

module Type where

import Common

import qualified Data.Set as S
import Control.Lens

-- Types

data Effect = AEGet Place -- get(ρ)
            | AEPut Place -- put(ρ)
            | AEVar Name -- effect variable ε
            deriving (Ord, Eq)

instance Show Effect where
    show (AEGet p) = "get(" ++ show p ++ ")"
    show (AEPut p) = "put(" ++ show p ++ ")"
    show (AEVar x) = x

newtype EVar = EVar { unEVar :: Name } deriving (Eq, Ord)

instance Show EVar where
    show (EVar x) = x

type Effects = S.Set Effect

emptyEffects :: Effects
emptyEffects = S.empty

data Type = TInt
          | TArrow DecoratedType (EVar, Effects) DecoratedType -- μ---ε.φ-->μ
          | TVar Name -- α
          deriving (Eq, Show)

data Place = PVar Name -- region variables
           | PReg Name -- region names
           deriving (Ord, Eq)

instance Show Place where
    show (PVar x) = x
    show (PReg x) = x

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

instance Canonicalizable CompoundTS where
    toCanonType (CTy ty) = CanonType [] [] [] ty
    toCanonType (CForallTy t sts)  = tvars %~ (t :) $ toCanonType sts
    toCanonType (CForallEff e sts) = evars %~ (e :) $ toCanonType sts
    toCanonType (CForallPlc p sts) = pvars %~ (p :) $ toCanonType sts

    fromCanonType (CanonType [] ts es ty) = foldr CForallTy (foldr CForallEff (CTy ty) es) ts
    fromCanonType c = error $ "can't transform" ++ show c ++ "into CompoundTS"

