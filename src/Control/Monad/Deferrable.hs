{-# LANGUAGE RankNTypes, LambdaCase #-}
module Control.Monad.Deferrable (
  DeferrableT,
  Deferrable,
  defer,
  fromFoldable,
  omega,
  bfs,
  toMaybe,
  runDeferrable,
  foldrS,
  foldrT,
  toList,
  toListOfN,
  toListWhile
 ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Functor.Identity
import qualified Data.Sequence as Sq

newtype QS m a = QS { uQS :: Sq.Seq (QS m a) -> m (Maybe (a, Sq.Seq (QS m a))) }

-- | A monad transformer which adds nondeterminism with a
-- depth-first/breadth-first hybrid search strategy. Searching within each layer
-- is done depth-first; boundaries between layers are inserted with the 'defer'
-- action; and execution of each layer does not begin until the layer before it
-- is finished.
newtype DeferrableT m a = D {
  unD :: forall r . (a -> QS m r) -> QS m r
 }
type Deferrable = DeferrableT Identity

instance Functor (DeferrableT m) where
  fmap f (D a) = D (a . (. f))

instance Applicative (DeferrableT m) where
  pure a = D ($ a)
  D f <*> D a = D (f . (a .) . (.))
  D a *> D b = D (a . const . b)

instance Monad (DeferrableT m) where
  D a >>= f = D (a . flip (unD . f))

instance Applicative m => Alternative (DeferrableT m) where
  empty = D (const $ QS (\q -> case Sq.viewl q of
    Sq.EmptyL -> pure Nothing
    a Sq.:< r -> uQS a r
   ))
  D a <|> D b = D (\c -> QS (\q ->
    uQS (a c) (b c Sq.<| q)
   ))

instance Applicative m => MonadPlus (DeferrableT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans DeferrableT where
  lift a = D (\c -> QS (\q -> a >>= flip uQS q . c))

-- | Create a level boundary.
defer :: DeferrableT m ()
defer = D (\c -> QS (\q -> case Sq.viewl q of
  Sq.EmptyL -> uQS (c ()) q
  a Sq.:< r -> uQS a (r Sq.|> c ())
 ))

fromFoldable :: (Foldable t, Applicative m) => t a -> DeferrableT m a
fromFoldable = foldr ((<|>) . pure) empty

-- | Returns the first element of the argument in the current layer; then each
-- subsequent element in a later layer.
omega :: (Foldable t, Applicative m) => t a -> DeferrableT m a
omega = foldr ((. (defer *>)) . (<|>) . pure) empty

bfs :: Monad m => (a -> m (Either [a] b)) -> a -> DeferrableT m b
bfs branch = go where
  go a = lift (branch a) >>= \case
    Left b -> defer >> fromFoldable b >>= go
    Right b -> return b

term :: Applicative m => a -> QS m a
term a = QS (\q -> pure (Just (a,q)))

-- | Extract the first value, if it exists.
toMaybe :: Applicative m => DeferrableT m a -> m (Maybe a)
toMaybe (D p) = fmap fst <$> uQS (p term) Sq.empty

foldrS :: Monad m => (a -> (s -> m r) -> s -> m r) -> (s -> m r) ->
  DeferrableT m a -> s -> m r
foldrS c z (D p) s0 = go (p term) Sq.empty s0 where
  go (QS x) q s = x q >>= \case
    Nothing -> z s
    Just (a,r) -> c a (case Sq.viewl r of
      Sq.EmptyL -> z
      y Sq.:< t -> go y t
     ) s

foldrT :: Monad m => (a -> m r -> m r) -> m r -> DeferrableT m a -> m r
foldrT c z = flip (foldrS ((.) . c) (const z)) ()

toList :: Monad m => DeferrableT m a -> m [a]
toList = foldrT (fmap . (:)) (return [])

runDeferrable :: Deferrable a -> [a]
runDeferrable = runIdentity . toList

toListOfN :: Monad m => Int -> DeferrableT m a -> m [a]
toListOfN n0 p = foldrS (\a r -> \case
  0 -> return []
  n -> (a:) <$> r (n - 1)
 ) (const $ return []) p n0

toListWhile :: Monad m => (a -> m Bool) -> DeferrableT m a -> m [a]
toListWhile p = foldrT (\a r -> p a >>= \case
  True -> (a:) <$> r
  False -> return []
 ) (return [])
