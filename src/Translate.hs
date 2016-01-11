module Translate where

import qualified Target as T
import qualified Source as S
import qualified Data.Map as M


-- Translate from the source language to target lambda calculus


-- Types
newtype EffVar = EffVar { unEffVar :: String } deriving (Eq, Show)

data AtomicEffect = AEffGet Name
                  | AEffPut Name
                  deriving (Show, Eq)

type ArrEff = (EffVar, AtomicEffect)

type AtomicEffects = S.Set AtomicEffect

noAtomicEffcts :: AtomicEffect
noAtomicEffcts = S.empty

data Type = TInt
          | TArrEff DecoratedType DecoratedType ArrEff
          | TVar Name
          deriving (Show, Eq)

type DecoratedType = (Type, Place)

-- For non-recursive
data SimpleTS = STy Type
              | SForallTy  Name SimpleTS
              | SForallEff Name SimpleTS
              deriving (Show, Eq)

-- For recursive
data CompoundTS = CTy Type
                | SForallTy  Name CompoundTS
                | SForallEff Name CompoundTS
                | SForallPlc Name CompoundTS
                deriving (Show, Eq)

-- TE ⊢ e ⇒ e′ : μ, ϕ
-- in TE, e translates to e′, which has type and place μ and effect ϕ

data TEState = TEState {
    _simpleDict   :: M.Map Name (SimpleTS, Place)
    _compoundDict :: M.Map Name (CompoundTS, Place)
}


check :: MonadState TEState m => S.Expr -> CompoundTS -> m (Maybe (T.Expr, CompoundTS, AtomicEffects))
check (S.EVar x) t = do
    (tx, p) <- typeOf x
    if t `isInstanceOf` tx
        then Just $ (T.EVar x, t, noAtomicEffcts)
        else Nothing




-- FIXME: comparation between simpleTS and CompoundTS
isInstanceOf :: CompoundTS -> CompoundTS -> Bool
isInstanceOf = undefined

