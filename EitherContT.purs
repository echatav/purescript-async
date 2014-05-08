module EitherContT where

import Control.Monad.Trans

data EitherContT r a m b = EitherContT ((a -> m r) -> (b -> m r) -> m r)
--Church encoding, EitherT a m b ~ forall r. EitherContT r a m b

runEitherContT :: forall r a m b. EitherContT r a m b -> (a -> m r) -> (b -> m r) -> m r
runEitherContT (EitherContT e) = e

instance functorEitherContT :: Functor (EitherContT r a m) where
  (<$>) f (EitherContT e) = EitherContT $ \kLeft kRight -> e kLeft (kRight <<< f)

instance applyEitherContT :: Apply (EitherContT r a m) where
  (<*>) (EitherContT f) (EitherContT e) = EitherContT $ \kLeft kRight ->
    f kLeft (\g -> e kLeft (kRight <<< g))

instance applicativeEitherContT :: Applicative (EitherContT r a m) where
  pure b = EitherContT $ \_ kRight -> kRight b

--throw is pure for the left branch
throw :: forall r a m b. a -> EitherContT r a m b
throw a = EitherContT $ \kLeft _ -> kLeft a

instance bindEitherContT :: Bind (EitherContT r a m) where
  (>>=) (EitherContT e) f = EitherContT $ \kLeft kRight ->
    e kLeft (\b -> runEitherContT (f b) kLeft kRight)

--catch is bind for the left branch
catch :: forall r m a a' b. EitherContT r a m b -> (a -> EitherContT r a' m b) -> EitherContT r a' m b
catch (EitherContT e) f = EitherContT $ \kLeft kRight ->
  e (\a -> runEitherContT (f a) kLeft kRight) kRight

instance monadEitherContT :: Monad (EitherContT r a m)

instance monadTransEitherContT :: MonadTrans (EitherContT r a) where
  lift m = EitherContT $ \_ kRight -> m >>= kRight
