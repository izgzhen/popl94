module Translate where

import qualified Target as T
import qualified Source as S
import Common
import Type

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S


-- Translate from the source language to target lambda calculus

-- TE ⊢ e ⇒ e′ : μ, ϕ
-- in TE, e translates to e′, which has type and place μ and effect ϕ

data TE = TE {
  _simpleDict   :: M.Map Name (SimpleTS, Place),
  _compoundDict :: M.Map Name (CompoundTS, Place)
}

data Substitution = Substitution {
  _placeSubst  :: M.Map Name Place
, _typeSubst   :: M.Map Name Type
, _effectSubst :: M.Map Name (Name, S.Set Effect)
}


isInstanceOf :: Type -> CompoundTS -> Maybe Substitution
isInstanceOf = undefined

