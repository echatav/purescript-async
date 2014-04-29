module Control.Monad.Async where

import Control.Monad.Eff

class (Monad m) <= MonadAsync m where
 apAsync :: forall a b.
            (((a -> b) -> m {}) -> m {})
         -> ((a -> m {}) -> m {})
         -> (b -> m {})
         -> m {}

foreign import apAsyncEff
  "function apAsyncEff (f) {\
  \  return function (x) {\
  \    return function (k) {\
  \      return function () {\
  \        var hf, hx;\
  \        f(function(f1) {\
  \          hf = f1;\
  \          if (hx) { return k(hf(hx)); }\
  \          else { return function () {}; }\
  \        })();\
  \        x(function(x1) {\
  \          hx = x1;\
  \          if (hf) {return k(hf(hx));}\
  \          else { return function () {}; }\
  \        })();\
  \};};};}"
  :: forall a b eff.
     (((a -> b) -> Eff eff {}) -> Eff eff {})
  -> ((a -> Eff eff {}) -> Eff eff {})
  -> (b -> Eff eff {})
  -> Eff eff {}

instance effAsync :: MonadAsync (Eff eff) where
 apAsync = apAsyncEff

data Async m a = Async ((a -> m {}) -> m {})
runAsync (Async x) = x

instance functorAsync :: Functor (Async m) where
 (<$>) f (Async x) = Async (\r -> x (f >>> r))

instance applyAsync :: (MonadAsync m) => Apply (Async m) where
 (<*>) (Async f) (Async a) = Async (apAsync f a)

instance applicativeAsync :: (MonadAsync m) => Applicative (Async m) where
 pure a = Async (\k -> k a)

instance bindAsync :: (MonadAsync m) => Bind (Async m) where
 (>>=) m k = Async (\c -> runAsync m (\a -> runAsync (k a) c))

instance monadAsync :: (MonadAsync m) => Monad (Async m)

liftAsync :: forall a m. (MonadAsync m) => m a -> Async m a
liftAsync m = Async (\f -> m >>= f)

foreign import timeout'
  "function timeout$prime(t){return function(e){return function(){setTimeout(e, t);};};}"
  :: forall a eff. Number -> Eff eff {} -> Eff eff {}

timeout :: forall eff. Number -> Async (Eff eff) {}
timeout n = Async (\k -> timeout' n (k {}))