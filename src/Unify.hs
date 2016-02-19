{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module Unify where

import Type
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens
import Common

data Substitution = Substitution {
  _placeSubst  :: M.Map Name Place
, _typeSubst   :: M.Map Name Type
, _effectSubst :: M.Map EVar (EVar, Effects)
} deriving (Show)

makeLenses ''Substitution

class Unify a where
    unify :: a -> a -> Maybe Substitution

instance Unify Type where
    unify TInt TInt  = Just emptySubst
    unify (TVar x) t = Just $ emptySubst { _typeSubst = M.singleton x t }
    unify t (TVar x) = Just $ emptySubst { _typeSubst = M.singleton x t }
    unify (TArrow (ty1,  p1)  (evar,  effs)  (ty2,  p2))
          (TArrow (ty1', p1') (evar', effs') (ty2', p2')) = do
          s <- unify ty1 ty1'
          s <- flip unionSubst s <$> unify (p1 `substIn` s) p1'
          s <- flip unionSubst s <$> unify (ty2 `substIn` s) ty2'
          s <- flip unionSubst s <$> unify (p2 `substIn` s) p2'
          let effs'' = effs `S.union` effs'
          return (effectSubst %~ (M.insert evar  (evar, effs'') .
                                  M.insert evar' (evar, effs'')) $ s)
    unify _ _        = Nothing

instance Unify Place where
    unify (PVar x) p = Just $ emptySubst { _placeSubst = M.singleton x p }
    unify p (PVar x) = Just $ emptySubst { _placeSubst = M.singleton x p }
    unify (PReg r1) (PReg r2) | r1 == r2 = Just emptySubst
    unify _ _ = Nothing

instance Unify DecoratedType where
    unify (ty, p) (ty', p') = do
        s <- unify ty ty'
        flip unionSubst s <$> unify (p `substIn` s) p'


unionSubst :: Substitution -> Substitution -> Substitution
unionSubst s2 s1 = Substitution (_placeSubst s2  `unionMap` _placeSubst s1)
                                (_typeSubst s2   `unionMap` _typeSubst s1)
                                (_effectSubst s2 `unionMap` _effectSubst s1)


unionMap :: Ord k => M.Map k a -> M.Map k a -> M.Map k a
unionMap a b = (b M.\\ a) `M.union` a

emptySubst :: Substitution
emptySubst = Substitution M.empty M.empty M.empty

class Subst a b where
    substIn :: a -> Substitution -> b

instance Subst Type Type where
    substIn (TVar x) subst =
        case M.lookup x (_typeSubst subst) of
            Just t  -> t
            Nothing -> TVar x
    substIn TInt _ = TInt

    substIn (TArrow decTy1 (evar, effs) decTy2) s =
        let (evar', effs') = case M.lookup evar (_effectSubst s) of
                Nothing  -> (evar, emptyEffects)
                Just ret -> ret
        in  TArrow (decTy1 `substIn` s)
                   (evar', effs' `S.union` effs)
                   (decTy2 `substIn` s)

instance Subst Place Place where
    substIn (PVar x) subst =
        case M.lookup x (_placeSubst subst) of
            Just p  -> p
            Nothing -> PVar x

instance Subst EVar (EVar, Effects) where
    substIn evar s = unsafeLookup' evar (_effectSubst s)
                        $ "substIn evar(" ++ show evar ++
                          ") s(" ++ show s ++ ") failed"

instance Subst Effects Effects where
    substIn effs s = S.unions $ map (flip substIn s) $ S.toList effs

instance Subst Effect Effects where
    substIn (AEGet p) s = S.singleton $ AEGet (p `substIn` s)
    substIn (AEPut p) s = S.singleton $ AEPut (p `substIn` s)
    substIn (AEVar x) s =
        let (EVar x', effs) = EVar x `substIn` s
        in  S.singleton (AEVar x) `S.union` effs

instance Subst DecoratedType DecoratedType where
    substIn (ty, p) s = (ty `substIn` s, p `substIn` s)

