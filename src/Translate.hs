module Translate where

import qualified Target as T
import Target (Place(..))
import qualified Source as S

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as Set


-- Translate from the source language to target lambda calculus


-- Types
newtype EffVar = EffVar { unEffVar :: String } deriving (Eq, Show)

type Name = String

data AtomicEffect = AEffGet Name
                  | AEffPut Name
                  deriving (Show, Eq)

type ArrEff = (EffVar, AtomicEffect)

type AtomicEffects = Set.Set AtomicEffect

noAtomicEffcts :: AtomicEffects
noAtomicEffcts = Set.empty

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
                | CForallTy  Name CompoundTS
                | CForallEff Name CompoundTS
                | CForallPlc Name CompoundTS
                deriving (Show, Eq)

-- TE ⊢ e ⇒ e′ : μ, ϕ
-- in TE, e translates to e′, which has type and place μ and effect ϕ

data TEState = TEState {
    _simpleDict   :: M.Map Name (SimpleTS, Place),
    _compoundDict :: M.Map Name (CompoundTS, Place)
}

check :: MonadState TEState m => S.Expr -> CompoundTS -> m (Maybe (T.Expr, CompoundTS, AtomicEffects))
check (S.EVar x) t = do
    (tx, p) <- typeOf x
    if t `isInstanceOf` tx
        then return $ Just $ (T.EVar x, t, noAtomicEffcts)
        else return Nothing

-- FIXME: comparation between simpleTS and CompoundTS
isInstanceOf :: CompoundTS -> CompoundTS -> Bool
isInstanceOf = undefined

typeOf :: MonadState TEState m => Name -> m (CompoundTS, Place)
typeOf = undefined
