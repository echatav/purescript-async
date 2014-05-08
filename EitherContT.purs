module EitherContT where

import Control.Monad.Trans

data EitherContT r a m b = EitherContT ((a -> m r) -> (b -> m r) -> m r)
--Church encoding, EitherT a m b ~ forall r. EitherContT r a m b

runEitherContT :: forall r a m b. EitherContT r a m b -> (a -> m r) -> (b -> m r) -> m r
runEitherContT (EitherContT m) = m

instance functorEitherContT :: Functor (EitherContT r a m) where
  (<$>) f (EitherContT m) = EitherContT $ \kLeft kRight -> m kLeft (kRight <<< f)

instance applyEitherContT :: Apply (EitherContT r a m) where
  (<*>) (EitherContT f) (EitherContT v) = EitherContT $ \kLeft kRight ->
    f kLeft $ \g -> v kLeft (kRight <<< g)

instance applicativeEitherContT :: Applicative (EitherContT r a m) where
  pure b = EitherContT $ \_ kRight -> kRight b

--throw is pure for the left branch
throw :: forall r a m b. a -> EitherContT r a m b
throw a = EitherContT $ \kLeft _ -> kLeft a

instance bindEitherContT :: Bind (EitherContT r a m) where
  (>>=) (EitherContT m) mf = EitherContT $ \kLeft kRight ->
    m kLeft (\b -> runEitherContT (mf b) kLeft kRight)

--catch is bind for the left branch
catch :: forall r m a a' b. EitherContT r a m b -> (a -> EitherContT r a' m b) -> EitherContT r a' m b
catch (EitherContT tryer) catcher = EitherContT $ \kLeft kRight ->
  tryer (\a -> runEitherContT (catcher a) kLeft kRight) kRight

instance monadEitherContT :: Monad (EitherContT r a m)

instance monadTransEitherContT :: MonadTrans (EitherContT r a) where
  lift m = EitherContT $ \kLeft kRight -> m >>= kRight
