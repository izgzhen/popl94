-- Translate from the source language to target lambda calculus
{-# LANGUAGE RecordWildCards, TypeSynonymInstances,
             FlexibleInstances, TemplateHaskell,
             UndecidableInstances, MultiParamTypeClasses #-}

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

initTEState :: TEState
initTEState = TEState {
  _tyDict     = M.empty
, _tyCounter  = 0
, _pCounter   = 0
, _effCounter = 0
}

type TE = ExceptT String (State TEState)

-- runTranslate :: S.Expr -> Either String T.Expr
runTranslate expr = evalState (runExceptT (translate' expr))

-- TE ⊢ e ⇒ e′ : μ, ϕ
-- in TE, e translates to e′, which has type and place μ and effect ϕ

translate :: S.Expr -> TE (T.Expr, DecoratedType, Effects, Substitution)

-- (T-INT)
translate (S.EInt i) = do
    p <- newPlaceVar
    return (T.EInt i p, (TInt, p), emptyEffects, emptySubst)

-- (T-VAR)
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
    let effs = eff1 `union` eff2 `union` effs' `union` S.singleton (AEGet p) -- FIXME: Not 100% original
    return ( T.EApp expr1' expr2', decTy `substIn` subst, effs
           , subst `unionSubst` subst2 `unionSubst` subst1)

-- (T-ABS)
translate (S.EAbs x expr) = do
    decTy1@(STy ty1, p1) <- (,) <$> (STy <$> newTyVar) <*> newPlaceVar
    (expr', decTy2, effs, subst) <- withSimpleType x decTy1 $ translate expr
    evar <- newEffVar
    p <- newPlaceVar
    return ( T.EAbs x expr' p, ( TArrow (ty1 `substIn` subst, p1 `substIn` subst)
                                        (evar, effs) decTy2
                               , p)
           , S.singleton (AEPut p), subst)

-- (T-LET)
translate (S.ELet x expr1 expr2) = do
    (expr1', (ty1, p1), eff1, subst1) <- translate expr1
    refineEnv subst1
    ty1' <- tyClosureSimple subst1 ty1
    (expr2', decTy, eff2, subst2) <- withSimpleType x (ty1', p1) $ translate expr2
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
    ty'' <- tyClosureCompound subst1 ty'
    (expr2', decTy, eff2, subst2)
        <- withCompoundType f (ty'', p') $ translate expr2
    return ( T.ELetrec f ps x p' expr1' expr2', decTy
           , eff1 `union` eff2, subst2 `unionSubst` subst1)

translate' :: S.Expr -> TE (T.Expr, DecoratedType, Effects, Substitution)
translate' expr = do
    (expr', decTy, eff, subst) <- translate expr
    let decTy' = decTy `substIn` subst
    let eff' = eff `substIn` subst
    eff'' <- observe decTy' eff'
    let ps = frv (eff' S.\\ eff'')
    return (foldr T.ELetReg (expr' `substIn` subst) ps, decTy', eff'', subst)

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
    S.fromList [ AEVar x        | x <- S.toList $ fev a ] `intersection`
    effects

class Fv a where
    -- Free region variables
    frv :: a -> S.Set Name

    -- Free type variables
    ftv :: a -> S.Set Name

    -- Free effect variables
    fev :: a -> S.Set Name

instance Fv a => Fv (S.Set a) where
    frv = S.unions . map frv . S.toList
    ftv = S.unions . map ftv . S.toList
    fev = S.unions . map fev . S.toList

instance (Fv a, Fv b) => Fv (a, b) where
    frv (a, b) = frv a `S.union` frv b
    ftv (a, b) = ftv a `S.union` ftv b
    fev (a, b) = fev a `S.union` fev b

instance Fv Effect where
    frv (AEGet (PVar x)) = S.singleton x
    frv (AEPut (PVar x)) = S.singleton x
    frv _ = S.empty

    ftv _ = S.empty

    fev (AEVar x) = S.singleton x
    fev _ = S.empty

instance Fv EVar where
    frv _ = S.empty
    ftv _ = S.empty
    fev (EVar x) = S.singleton x

instance Fv Place where
    frv (PVar x) = S.singleton x
    frv _ = S.empty
    ftv _ = S.empty
    fev _ = S.empty

instance Fv Type where
    frv (TArrow decTy1 arrEff decTy2) =
        frv decTy1 `S.union`
        frv arrEff `S.union`
        frv decTy2
    frv _ = S.empty

    ftv (TVar x) = S.singleton x
    ftv (TArrow (ty1, _) _ (ty2, _)) = ftv ty1 `S.union` ftv ty2

    fev (TArrow decTy1 arrEff decTy2) =
        fev decTy1 `S.union`
        fev arrEff `S.union`
        fev decTy2
    fev _ = S.empty

instance Fv x => Fv (M.Map k x) where
    frv m = S.unions . map frv $ M.elems m
    fev m = S.unions . map fev $ M.elems m
    ftv m = S.unions . map ftv $ M.elems m

instance (Fv a, Fv b) => Fv (Either a b) where
    frv (Left a)  = frv a
    frv (Right b) = frv b
    fev (Left a)  = fev a
    fev (Right b) = fev b
    ftv (Left a)  = ftv a
    ftv (Right b) = ftv b

instance Fv CompoundTS where
    frv ts = let c = toCanonType ts
             in  frv (_innerty c) S.\\ S.fromList (_pvars c)

    ftv ts = let c = toCanonType ts
             in  frv (_innerty c) S.\\ S.fromList (_tvars c)

    fev ts = let c = toCanonType ts
             in  frv (_innerty c) S.\\ S.fromList (_evars c)

instance Fv SimpleTS where
    ftv ts = let c = toCanonType ts
             in  frv (_innerty c) S.\\ S.fromList (_tvars c)

    fev ts = let c = toCanonType ts
             in  frv (_innerty c) S.\\ S.fromList (_evars c)

    frv ts = let c = toCanonType ts
             in  frv (_innerty c) S.\\ S.fromList (_evars c)

refineEnv :: Substitution -> TE ()
refineEnv s = tyDict %= flip (foldr f) (M.toList $ _typeSubst s)
    where
        f (x, ty) dict =
            case unsafeLookup x dict of
                (Right _, p) -> M.insert x (Right (CTy ty), p) dict
                (Left  _, p) -> M.insert x (Left  (STy ty), p) dict

tyClosureSimple :: Substitution -> Type -> TE SimpleTS
tyClosureSimple subst ty = do
    oldDict <- use tyDict
    refineEnv subst
    newDict <- use tyDict

    let ts = S.toList $ ftv ty S.\\ ftv newDict
    let es = S.toList $ fev ty S.\\ fev newDict

    tyDict .= oldDict
    return $ fromCanonType (CanonType [] ts es ty)

tyClosureCompound :: Substitution -> Type -> TE CompoundTS
tyClosureCompound subst ty = do
    oldDict <- use tyDict
    refineEnv subst
    newDict <- use tyDict

    let ps = S.toList $ frv ty S.\\ frv newDict
    let ts = S.toList $ ftv ty S.\\ ftv newDict
    let es = S.toList $ fev ty S.\\ fev newDict

    tyDict .= oldDict
    return $ fromCanonType (CanonType ps ts es ty)


instance Subst T.Expr T.Expr where
    substIn (T.EInt i p) s        = T.EInt i (p `substIn` s)
    substIn (T.EVar x) _          = T.EVar x
    substIn (T.EAbs x e p) s      = T.EAbs x (e `substIn` s) (p `substIn` s)
    substIn (T.EApp e1 e2) s      = T.EApp (e1 `substIn` s) (e2 `substIn` s)
    substIn (T.ELet x e1 e2) s    = T.ELet x (e1 `substIn` s) (e2 `substIn` s)
    substIn (T.ELetrec f ps x p e1 e2) s =
        T.ELetrec f (map (\(PVar p) -> p) $ map (\p -> (PVar p) `substIn` s) ps)
                x (p `substIn` s) (e1 `substIn` s) (e2 `substIn` s)
    substIn (T.EFun f ps p) s     =
        T.EFun f (map (flip substIn s) ps) (p `substIn` s)
    substIn (T.ELetReg x e) s     = T.ELetReg x (e `substIn` s)
