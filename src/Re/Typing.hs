{-# LANGUAGE LambdaCase #-}

module Re.Typing where

import qualified Re.Source as S
import qualified Re.Target as T
import Re.Target (Basis, RegVar, InstList)
import Re.Source (TS)
import Re.Common

import qualified Data.Map as M
import Data.Set (empty, fromList)

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
algoS :: Basis -> Basis -> S.Expr -> TE (Substitution, Basis, Basis, T.Term)
algoS a b = \case
    S.EVar x tys -> do
        (ts, reg) <- typeOf x
        bound <- letrecBound x
        if bound
            then do
                (s, a1, ty, il) <- newInstance a ts tys
                reg' <- freshRegVar a
                return ( s, a1 |+| [reg']
                       , s `subst` (b |+| (a1 |\| a)) |+| [reg']
                       , T.Term (T.EVar x) (ty, reg') (fromList [Left reg, Left reg']))
            else do
                (ts, reg) <- typeOf x
                let ty = fromTS ts
                return ( idSubst, a, b
                       , T.Term (T.EVar x) (ty, reg) empty)




-- Algorithm R
algoR :: Basis -> T.Term -> TE (Substitution, Basis, T.Term)
algoR = undefined

typeOf :: Name -> TE (TS, RegVar)
typeOf = undefined


idSubst :: Substitution
idSubst = undefined

(|+|) :: Basis -> [RegVar] -> Basis
(|+|) = undefined


(|\|) :: Basis -> Basis -> [RegVar]
(|\|) = undefined


letrecBound :: Name -> TE Bool
letrecBound = undefined

newInstance :: Basis -> TS -> [S.Type] -> TE (Substitution, Basis, T.Type, InstList)
newInstance = undefined

freshRegVar :: Basis -> TE RegVar
freshRegVar = undefined

subst :: Substitution -> Basis -> Basis
subst = undefined

fromTS :: TS -> T.Type
fromTS = undefined
