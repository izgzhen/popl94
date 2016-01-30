-- Translate from the source language to target lambda calculus
{-# LANGUAGE RecordWildCards, TypeSynonymInstances,
             FlexibleInstances #-}

module Translate where

import qualified Target as T
import qualified Source as S
import Common
import Type
import Unify

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (union, intersection)

type TEState = M.Map Name (Either SimpleTS CompoundTS, Place)

type TE = ExceptT String (State TEState)

-- TE ⊢ e ⇒ e′ : μ, ϕ
-- in TE, e translates to e′, which has type and place μ and effect ϕ

-- (T-VAR)
translate :: S.Expr -> TE (T.Expr, DecoratedType, Effects, Substitution)
translate (S.EVar x) = do
    (sty, p) <- lookupSimple x
    (ty, subst) <- instSimple sty
    return (T.EVar x, (ty, p), emptyEffects, subst)

-- (T-FUN)
translate (S.EFun f) = do
    (cts, p') <- lookupCompound f
    let c = toCanonType cts
    (ty, subst) <- instCompound cts
    let ps = map (flip substIn subst) $ map PVar (_pvars c)
    p <- newPlaceVar
    let effs = S.fromList [AEGet p', AEPut p]
    return (T.EFun f ps p, (ty, p), effs, subst)

-- (T-APP)
translate e@(S.EApp expr1 expr2) = do
    (expr1', decTy1, eff1, subst1) <- translate expr1
    refineEnv subst1
    (expr2', decTy2, eff2, subst2) <- translate expr2
    evar <- newEffVar
    decTy <- (,) <$> newTyVar <*> newPlaceVar
    p <- newPlaceVar
    subst <- fromJust $ unify decTy1 (TArrow decTy2 (evar, emptyEffects) decTy, p)
    let (evar', effs') = evar `substIn` subst :: (EVar, Effects)
    let effs = eff1 `union` eff2 `union` effs' -- FIXME: Not 100% original
    return ( T.EApp expr1' expr2', decTy `substIn` subst, effs
           , subst `unionSubst` subst2 `unionSubst` subst1)

-- (T-ABS)
translate (S.EAbs x expr) = do
    decTy1@(STy ty1, p1) <- (,) <$> (STy <$> newTyVar) <*> newPlaceVar
    (expr', decTy2, effs, subst) <- withSimpleType x decTy1 $ translate expr
    evar <- newEffVar
    p <- newPlaceVar
    return ( T.EAbs x expr' p, (TArrow (ty1 `substIn` subst, p1 `substIn` subst)
                                       (evar, effs) decTy2
                               , p)
           , S.singleton (AEPut p), subst)

-- (T-LET)
translate (S.ELet x expr1 expr2) = do
    (expr1', (ty1, p1), eff1, subst1) <- translate expr1
    refineEnv subst1
    ty1' <- tyClosure subst1 ty1
    (expr2', decTy, eff2, subst2) <- withSimpleType x (STy ty1', p1) $ translate expr2
    return ( T.ELet x expr1' expr2', decTy
           , eff1 `union` eff2, subst2 `unionSubst` subst1)

-- (T-LETREC)
translate (S.ELetrec f x expr1 expr2) = do
    ty <- newTyVar
    p <- newPlaceVar
    (T.EAbs x expr1' p', (ty', _), eff1, subst1)
        <- withCompoundType f (CTy ty, p) $ translate (S.EAbs x expr1)
    let ps = frv (ty `substIn` subst1 :: Type)
    refineEnv subst1
    ty'' <- tyClosure subst1 ty'
    (expr2', decTy, eff2, subst2)
        <- withCompoundType f (CTy ty'', p') $ translate expr2
    return ( T.ELetrec f ps x p' expr1' expr2', decTy
           , eff1 `union` eff2, subst2 `unionSubst` subst1)

translate' :: S.Expr -> TE (T.Expr, DecoratedType, Effects, Substitution)
translate' expr = do
    (expr', decTy, eff, subst) <- translate expr
    eff' <- observe decTy eff
    let ps = frv (eff S.\\ eff')
    return (foldr T.ELetReg expr' ps, decTy, eff', subst)

-- -- Utils

lookupSimple :: Name -> TE (SimpleTS, Place)
lookupSimple = undefined

lookupCompound :: Name -> TE (CompoundTS, Place)
lookupCompound = undefined

newTyVar :: TE Type
newTyVar = undefined

newEffVar :: TE EVar
newEffVar = undefined

newPlaceVar :: TE Place
newPlaceVar = undefined

withCompoundType :: Name -> (CompoundTS, Place) -> TE a -> TE a
withCompoundType = undefined

withSimpleType :: Name -> (SimpleTS, Place) -> TE a -> TE a
withSimpleType = undefined

instSimple :: SimpleTS -> m (Type, Substitution)
instSimple = undefined

instCompound :: CompoundTS -> m (Type, Substitution)
instCompound = undefined

observe :: Fv a => a -> Effects -> TE Effects
observe a effects = return $
    S.fromList [ AEPut (PVar p) | p <- frv a ] `union`
    S.fromList [ AEGet (PVar p) | p <- frv a ] `union`
    S.fromList [ AEVar x | x <- fev a ] `intersection`
    effects

class Fv a where
    -- Free region variables
    frv :: a -> [Name]

    -- Free effect variables
    fev :: a -> [Name]

instance Fv Effects where
    frv = undefined
    fev = undefined

instance Fv DecoratedType where
    frv = undefined
    fev = undefined

instance Fv Type where
    frv = undefined
    fev = undefined


refineEnv :: Substitution -> TE ()
refineEnv = undefined

tyClosure :: Substitution -> Type -> TE Type
tyClosure = undefined


fromJust :: MonadError String m => Maybe a -> m a
fromJust (Just a) = return a
fromJust Nothing  = throwError "fromJust Nothing"

