{-# LANGUAGE RankNTypes #-}
module Control.Monad.Deferrable (
  DeferrableT,
  Deferrable,
  defer,
  fromFoldable,
  omega
 ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Functor.Identity
import qualified Data.Sequence as Sq

newtype QS m a = QS { uQS :: Sq.Seq (QS m a) -> m (Maybe (a, Sq.Seq (QS m a))) }
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

defer :: DeferrableT m ()
defer = D (\c -> QS (\q -> case Sq.viewl q of
  Sq.EmptyL -> uQS (c ()) q
  a Sq.:< r -> uQS a (q Sq.|> c ())
 ))

fromFoldable :: (Foldable t, Applicative m) => t a -> DeferrableT m a
fromFoldable l = D (\c -> QS (\q ->
  case Sq.viewl $ foldMap (Sq.singleton . c) l <> q of
    Sq.EmptyL -> pure Nothing
    a Sq.:< r -> uQS a r
 ))

omega :: (Foldable t, Applicative m) => t a -> DeferrableT m a
omega = foldr ((. (defer *>)) . (<|>) . pure) empty
