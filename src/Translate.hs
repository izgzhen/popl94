-- Translate from the source language to target lambda calculus
{-# LANGUAGE RecordWildCards, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module Translate where

import qualified Target as T
import qualified Source as S
import Common
import Type

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S

data TEState = TEState {
  _typeOf   :: M.Map Name (Either SimpleTS CompoundTS, Place)
}

type TE = ExceptT String (State TEState)

data Substitution = Substitution {
  _placeSubst  :: M.Map Name Place
, _typeSubst   :: M.Map Name Type
, _effectSubst :: M.Map Name (Name, Effects)
}

class Subst a b where
    substIn :: MonadError String m => a -> Substitution -> m b

instance Subst Type Type where
    substIn = undefined

instance Subst Place Place where
    substIn = undefined

instance Subst Name (Name, Effects) where
    substIn = undefined

-- TE ⊢ e ⇒ e′ : μ, ϕ
-- in TE, e translates to e′, which has type and place μ and effect ϕ

translate :: S.Expr -> TE (T.Expr, DecoratedType, Effects, Substitution)
translate (S.EVar x) = do
    (sty, p) <- lookupSimple x
    let c@CanonType{..} = toCanonType sty
    ts <- sequence $ replicate (length _tvars) newTyVar
    es <- sequence $ replicate (length _evars) newEffVar
    let subst = Substitution M.empty
                             (M.fromList $ zip _tvars ts)
                             (M.fromList $ zip _evars (map (\e -> (e, emptyEffects)) es))
    return (T.EVar x, (_innerty, p), emptyEffects, subst)

translate (S.EAbs x expr) = do
    ty1 <- newTyVar
    p1 <- newPlaceVar
    (expr', decTy2, eff, subst) <- withCompoundType x (CTy ty1, p1) $ translate expr
    ty1' <- ty1 `substIn` subst
    p1' <- p1 `substIn` subst
    let decTy1 = (ty1', p1')
    evar <- newEffVar
    p@(PVar px) <- newPlaceVar
    return (T.EAbs x expr' p, (TArrow decTy1 (evar, eff) decTy2, p), S.singleton (AEPut px), subst)

-- Utils

isInstanceOf :: Type -> CompoundTS -> Maybe Substitution
isInstanceOf = undefined


lookupSimple :: Name -> TE (SimpleTS, Place)
lookupSimple = undefined

newTyVar :: TE Type
newTyVar = undefined

newEffVar :: TE Name
newEffVar = undefined

newPlaceVar :: TE Place
newPlaceVar = undefined

withCompoundType :: Name -> (CompoundTS, Place) -> TE a -> TE a
withCompoundType = undefined



