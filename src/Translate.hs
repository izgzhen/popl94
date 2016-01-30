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
import Data.Set (union)

data TEState = TEState {
  _typeOf   :: M.Map Name (Either SimpleTS CompoundTS, Place)
}

type TE = ExceptT String (State TEState)

data Substitution = Substitution {
  _placeSubst  :: M.Map Name Place
, _typeSubst   :: M.Map Name Type
, _effectSubst :: M.Map Name (Name, Effects)
}

unionSubst :: Substitution -> Substitution -> Substitution
unionSubst = undefined

emptySubst :: Substitution
emptySubst = undefined

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
    p <- newPlaceVar
    return (T.EAbs x expr' p, (TArrow decTy1 (evar, eff) decTy2, p), S.singleton (AEPut p), subst)

translate e@(S.EApp expr1 expr2) = do
    (expr1', (ty1, p), eff1, subst1) <- translate expr1
    (expr2', decTy2,   eff2, subst2) <- translate expr2
    case ty1 of
        TArrow decTy2' (evar, eff0) decTy
            | decTy2 == decTy2' -> do
                let eff = eff0 `union` eff1 `union` eff2 `union` S.singleton (AEGet p)
                return (T.EApp expr1' expr2', decTy, eff, subst1 `unionSubst` subst2)
        _ -> throwError $ "can't type " ++ show e

translate (S.ELet x expr1 expr2) = do
    (expr1', (ty1, p1), eff1, subst1) <- translate expr1
    tyx <- newTyVar
    (expr2', decTy, eff2, subst2) <- withSimpleType x (STy tyx, p1) $ translate expr2
    return (T.ELet x expr1' expr2', decTy, eff1 `S.union` eff2, subst1 `unionSubst` subst2)

translate (S.ELetrec f x expr1 expr2) = do
    ty <- newTyVar
    p <- newPlaceVar
    (T.EAbs x expr1' p', (_, p), eff1, subst1)
        <- withCompoundType f (CTy ty, p)
            $ translate (S.EAbs x expr1)
    when (p /= p') $ throwError "p' /= p"
    c1 <- undefined
    ty' <- newTyVar
    (expr2', decTy, eff2, subst2)
        <- withCompoundType f (CTy ty', p) $ translate expr2
    return $ ( T.ELetrec f (_pvars c1) x p expr1' expr2'
             , decTy
             , eff1 `union` eff2
             , subst1 `unionSubst` subst2 )

translate (S.EFun f) = do
    (cts, p') <- lookupCompound f
    let c = toCanonType cts
    (ty, subst) <- getSubst cts
    p <- newPlaceVar
    let eff = S.fromList [AEGet p', AEPut p]
    ps <- mapM (flip substIn subst) (map PVar $ _pvars c)
    return (T.EFun f ps p, (ty, p), eff, subst)

translate' :: S.Expr -> TE (T.Expr, DecoratedType, Effects, Substitution)
translate' expr = do
    (expr', decTy, eff, subst) <- translate expr
    eff' <- observe decTy eff
    let ps = frv (eff S.\\ eff')
    return (foldr T.ELetReg expr' ps, decTy, eff', emptySubst)

-- Utils

isInstanceOf :: Type -> CompoundTS -> Maybe Substitution
isInstanceOf = undefined


lookupSimple :: Name -> TE (SimpleTS, Place)
lookupSimple = undefined

lookupCompound :: Name -> TE (CompoundTS, Place)
lookupCompound = undefined

newTyVar :: TE Type
newTyVar = undefined

newEffVar :: TE Name
newEffVar = undefined

newPlaceVar :: TE Place
newPlaceVar = undefined

withCompoundType :: Name -> (CompoundTS, Place) -> TE a -> TE a
withCompoundType = undefined

withSimpleType :: Name -> (SimpleTS, Place) -> TE a -> TE a
withSimpleType = undefined

getSubst :: (Canonicalizable a, MonadError String m) =>
            a -> m (Type, Substitution)
getSubst = undefined


observe :: DecoratedType -> Effects -> m Effects
observe = undefined


frv :: Effects -> [Name]
frv = undefined

