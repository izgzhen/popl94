-- Translate from the source language to target lambda calculus
{-# LANGUAGE RecordWildCards, TypeSynonymInstances,
             FlexibleInstances, TemplateHaskell #-}

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
import Control.Lens

data TEState = TEState {
  _tyDict       :: M.Map Name (Either SimpleTS CompoundTS, Place)
, _tyCounter    :: Int
, _pCounter     :: Int
, _effCounter   :: Int
}

makeLenses ''TEState

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
    let ps = S.toList $ frv (ty `substIn` subst1 :: Type)
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
lookupSimple x = do
    dict <- _tyDict <$> get
    case M.lookup x dict of
        Just (Left sts, p) -> return (sts, p)
        _ -> throwError $ "can't find SimpleTS " ++ x

lookupCompound :: Name -> TE (CompoundTS, Place)
lookupCompound x = do
    dict <- _tyDict <$> get
    case M.lookup x dict of
        Just (Right cts, p) -> return (cts, p)
        _ -> throwError $ "can't find CompoundTS " ++ x

newTyVar :: TE Type
newTyVar = do
    i <- use tyCounter
    tyCounter .= i + 1
    return $ TVar ("_ty" ++ show i)

newEffVar :: TE EVar
newEffVar = do
    i <- use effCounter
    effCounter .= i + 1
    return $ EVar ("_eff" ++ show i)

newPlaceVar :: TE Place
newPlaceVar = do
    i <- use pCounter
    pCounter .= i + 1
    return $ PVar ("_p" ++ show i)

withCompoundType :: Name -> (CompoundTS, Place) -> TE a -> TE a
withCompoundType x (cts, p) action = do
    oldDict <- use tyDict
    tyDict %= M.insert x (Right cts, p)
    ret <- action
    tyDict .= oldDict
    return ret

withSimpleType :: Name -> (SimpleTS, Place) -> TE a -> TE a
withSimpleType x (sts, p) action = do
    oldDict <- use tyDict
    tyDict %= M.insert x (Left sts, p)
    ret <- action
    tyDict .= oldDict
    return ret

instSimple :: SimpleTS -> TE (Type, Substitution)
instSimple sts = do
    let c@CanonType{..} = toCanonType sts
    ts <- sequence (replicate (length _tvars) newTyVar)
    es <- sequence (replicate (length _evars) newEffVar)
    let subst = emptySubst {
      _typeSubst   = M.fromList $ zip _tvars ts
    , _effectSubst = M.fromList $ zip (map EVar _evars) (map (\e -> (e, emptyEffects)) es)
    }
    return (_innerty, subst)

instCompound :: CompoundTS -> TE (Type, Substitution)
instCompound cts = do
    let c@CanonType{..} = toCanonType cts
    ts <- sequence (replicate (length _tvars) newTyVar)
    es <- sequence (replicate (length _evars) newEffVar)
    ps <- sequence (replicate (length _pvars) newPlaceVar)
    let subst = emptySubst {
      _typeSubst   = M.fromList $ zip _tvars ts
    , _effectSubst = M.fromList $ zip (map EVar _evars) (map (\e -> (e, emptyEffects)) es)
    , _placeSubst  = M.fromList $ zip _pvars ps
    }
    return (_innerty, subst)

observe :: Fv a => a -> Effects -> TE Effects
observe a effects = return $
    S.fromList [ AEPut (PVar p) | p <- S.toList $ frv a ] `union`
    S.fromList [ AEGet (PVar p) | p <- S.toList $ frv a ] `union`
    S.fromList [ AEVar x | x <- S.toList $ fev a ] `intersection`
    effects

class Fv a where
    -- Free region variables
    frv :: a -> S.Set Name

    -- Free effect variables
    fev :: a -> S.Set Name

instance Fv a => Fv (S.Set a) where
    frv = S.unions . map frv . S.toList
    fev = S.unions . map fev . S.toList

instance (Fv a, Fv b) => Fv (a, b) where
    frv (a, b) = frv a `S.union` frv b
    fev (a, b) = fev a `S.union` fev b

instance Fv Effect where
    frv (AEGet (PVar x)) = S.singleton x
    frv (AEPut (PVar x)) = S.singleton x
    frv _ = S.empty

    fev (AEVar x) = S.singleton x
    fev _ = S.empty

instance Fv EVar where
    frv _ = S.empty
    fev (EVar x) = S.singleton x

instance Fv Place where
    frv (PVar x) = S.singleton x
    frv _ = S.empty

    fev _ = S.empty

instance Fv Type where
    frv (TArrow decTy1 arrEff decTy2) =
        frv decTy1 `S.union`
        frv arrEff `S.union`
        frv decTy2
    frv _ = S.empty

    fev (TArrow decTy1 arrEff decTy2) =
        fev decTy1 `S.union`
        fev arrEff `S.union`
        fev decTy2
    fev _ = S.empty


refineEnv :: Substitution -> TE ()
refineEnv s = tyDict %= flip (foldr f) (M.toList $ _typeSubst s)
    where
        f (x, ty) dict =
            case unsafeLookup x dict of
                (Right _, p) -> M.insert x (Right (CTy ty), p) dict
                (Left  _, p) -> M.insert x (Left  (STy ty), p) dict

tyClosure :: Substitution -> Type -> TE Type
tyClosure subst ty = undefined


fromJust :: MonadError String m => Maybe a -> m a
fromJust (Just a) = return a
fromJust Nothing  = throwError "fromJust Nothing"


unsafeLookup :: (Ord k, Show k) => k -> M.Map k v -> v
unsafeLookup k m = case M.lookup k m of
    Just v -> v
    Nothing -> error $ "can't find " ++ show k

