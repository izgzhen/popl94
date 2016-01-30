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
}

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
unionSubst = undefined

emptySubst :: Substitution
emptySubst = undefined

class Subst a b where
    substIn :: a -> Substitution -> b

instance Subst Type Type where
    substIn = undefined

instance Subst Place Place where
    substIn = undefined

instance Subst EVar (EVar, Effects) where
    substIn = undefined

instance Subst DecoratedType DecoratedType where
    substIn (ty, p) s = (ty `substIn` s, p `substIn` s)
