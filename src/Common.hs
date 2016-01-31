module Common where

import Control.Monad.Except
import qualified Data.Map as M

type Name = String

fromJust :: MonadError String m => Maybe a -> m a
fromJust (Just a) = return a
fromJust Nothing  = throwError "fromJust Nothing"


unsafeLookup :: (Ord k, Show k) => k -> M.Map k v -> v
unsafeLookup k m = case M.lookup k m of
    Just v -> v
    Nothing -> error $ "can't find " ++ show k
