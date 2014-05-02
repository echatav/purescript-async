module Async where

import Control.Monad.Eff
import Control.Monad.Cont.Trans

foreign import data Async :: !

foreign import timeout
  "function timeout(n){return function(f){return function(){setTimeout(f,n);};};}"
  :: forall eff. Number -> Eff (async :: Async | eff) {} -> Eff (async :: Async | eff) {}

timeoutCont :: forall eff. Number -> ContT {} (Eff (async :: Async | eff)) {}
timeoutCont n = ContT (\k -> timeout n (k {}))

foreign import apAsync
  "function apAsync (f) {\
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
  \          if (hf) { return k(hf(hx)); }\
  \          else { return function () {}; }\
  \        })();\
  \};};};}"
  :: forall a b eff.
     (((a -> b) -> Eff (async :: Async | eff) {}) -> Eff (async :: Async | eff) {})
  -> ((a -> Eff (async :: Async | eff) {}) -> Eff (async :: Async | eff) {})
  -> (b -> Eff (async :: Async | eff) {})
  -> Eff (async :: Async | eff) {}

mapAsync :: forall a b eff.
            (a  -> ContT {} (Eff (async :: Async | eff))  b)
         -> [a] -> ContT {} (Eff (async :: Async | eff)) [b]
mapAsync f [] = return []
mapAsync f (a:as) = ((:) <$> (f a)) `ap` (mapAsync f as)
 where ap (ContT g) (ContT x) = ContT (apAsync g x)
