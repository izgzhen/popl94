{-# LANGUAGE LambdaCase, TypeSynonymInstances, FlexibleInstances #-}

module Re.Typing where

import qualified Re.Source as S
import qualified Re.Target as T
import Re.Target (Basis, RegVar, InstList, Term(..), EffVar, Effect)
import Re.Source (TS)
import Re.Common

import qualified Data.Map as M
import Data.Set (empty, fromList, singleton, union)

import Control.Monad.State
import Control.Monad.Except

data TypeEnv = TypeEnv {
  _ts  :: M.Map Name S.TS
, _reg :: M.Map Name T.RegVar
} deriving (Show, Eq)


type Cone = (Basis, Basis)

type TE = ExceptT String (State TypeEnv)

type Substitution = Int

-- Algorithm S
algoS :: Basis -> Basis -> S.Expr -> TE (Substitution, Basis, Basis, Term)
algoS a b = \case
    S.EVar x tys -> do
        (ts, reg) <- typeOf x
        bound <- letrecBound x
        if bound
            then do
                (s, a1, ty, il) <- newInstance a ts tys
                reg' <- freshRegVar a
                return ( s, a1 |+| (singleton reg', empty)
                       , s `subst` (b |+| (a1 |\| a)) |+| (singleton reg', empty)
                       , Term (T.EVar x) (ty, reg') (fromList [Left reg, Left reg']))
            else do
                (ts, reg) <- typeOf x
                let ty = fromTS ts
                return ( idSubst, a, b
                       , Term (T.EVar x) (ty, reg) empty)
    S.EAbs x ty0 e1 -> do
        (a0, annTy0) <- freshTypeWithPlace a ty0
        
        (s1, a1, b1, t1@(Term e1' annTy1 eff1)) <-
            withValType x ty0 $ algoS a0 (b |+| (a0 |\| a)) e1

        e <- freshEffVar a1
        reg <- freshRegVar a1
        let a2 = (singleton reg, singleton (e, eff1))
        let s = toSubst e (e, eff1) `compose` s1

        return ( s, a1 |+| (singleton reg, singleton (e, empty))
               , b1 |+| a2, Term (T.EAbs x (s1 `subst` annTy0) t1)
                                 (T.TArrow (s1 `subst` annTy0)
                                           (e, eff1)
                                           annTy1
                                 , reg)
                                 (singleton (Left reg)))
    S.EApp e1 e2 -> do
        (s1, a1, b1, t1@(Term e1 (T.TArrow annTy2 (e, eff0) annTy1, reg0) eff1))
            <- algoS a b e1
        (s2, a2, b2, t2@(Term e2 annTy2' eff2))
            <- withSubst s1 $ algoS a1 b1 e2
        let s3 = unifyMu (s2 `subst` annTy2) annTy2'
        let s = s3 `compose` s2 `compose` s1
        retract ( s, a2, s3 `subst` b2, s `subst` b
                , Term (s3 `subst` (T.EApp (s2 `subst` t1) t2))
                       (s3 `subst` (s2 `subst` annTy1))
                       (s3 `subst` ((s2 `subst` (fromList [Right e, Left reg0] `union`
                                                 eff0 `union`
                                                 eff1))
                                        `union` eff2)))
        


-- Algorithm R
algoR :: Basis -> Term -> TE (Substitution, Basis, Term)
algoR = undefined

typeOf :: Name -> TE (TS, RegVar)
typeOf = undefined


idSubst :: Substitution
idSubst = undefined

-- disjoint union
(|+|) :: Basis -> Basis -> Basis
(|+|) = undefined

-- component-wise difference
(|\|) :: Basis -> Basis -> Basis
(|\|) = undefined


letrecBound :: Name -> TE Bool
letrecBound = undefined

newInstance :: Basis -> TS -> [S.Type] -> TE (Substitution, Basis, T.Type, InstList)
newInstance = undefined

freshRegVar :: Basis -> TE RegVar
freshRegVar = undefined

freshEffVar :: Basis -> TE EffVar
freshEffVar = undefined

class Substituted a where
    subst :: Substitution -> a -> a

instance Substituted Basis where
    subst = undefined

instance Substituted T.AnnTy where
    subst = undefined

instance Substituted Term where
    subst = undefined

instance Substituted Effect where
    subst = undefined

instance Substituted T.Expr where
    subst = undefined

fromTS :: TS -> T.Type
fromTS = undefined

freshTypeWithPlace :: Basis -> S.Type -> TE (Basis, T.AnnTy)
freshTypeWithPlace = undefined

withValType :: Name -> S.Type -> TE a -> TE a
withValType = undefined

compose :: Substitution -> Substitution -> Substitution
compose = undefined

toSubst :: EffVar -> (EffVar, Effect) -> Substitution
toSubst = undefined

withSubst :: Substitution -> TE a -> TE a
withSubst = undefined

unifyMu :: T.AnnTy -> T.AnnTy -> Substitution
unifyMu = undefined

retract :: (Substitution, Basis, Basis, Basis, Term) ->
           TE (Substitution, Basis, Basis, Term)
retract = undefined

