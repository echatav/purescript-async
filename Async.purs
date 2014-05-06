module Async where

import Control.Monad.Eff
import Control.Monad.Cont.Trans
import EitherContT
import Debug.Trace

foreign import data Async :: *

foreign import timeout
  "var timeout=function(n){return function(f){return function(){setTimeout(f,n);};};}"
  :: forall a eff. Number -> a -> Eff eff Async

timeoutCont :: forall eff. Number -> ContT Async (Eff eff) {}
timeoutCont n = ContT (\k -> timeout n (k {}))

foreign import traceAsync
  "var traceAsync=function(x){return function(){console.log(x);};}"
  :: forall eff. String -> Eff (trace :: Trace | eff) Async

printAsync :: forall a eff. (Show a) => a -> Eff (trace :: Trace | eff) Async
printAsync = traceAsync <<< show

foreign import apAsync
  "var apAsync=function(f){\
  \     return function(x){\
  \     return function(k){\
  \     return function( ){\
  \ var hf, hx;\
  \ f(function(f1){\
  \  hf = f1;\
  \  if(hx){return k(hf(hx));}\
  \  else{return function(){};}\
  \ })();\
  \ x(function(x1){\
  \  hx = x1;\
  \  if(hf){return k(hf(hx));}\
  \  else{return function(){};}\
  \ })();\
  \};};};}"
  :: forall a b eff.
     (((a -> b) -> Eff eff Async) -> Eff eff Async)
  -> ((   a     -> Eff eff Async) -> Eff eff Async)
  -> (    b     -> Eff eff Async) -> Eff eff Async

mapAsync :: forall a b eff.
            (a  -> ContT Async (Eff eff)  b)
         -> [a] -> ContT Async (Eff eff) [b]
mapAsync f [] = return []
mapAsync f (a:as) = ap ((:) <$> (f a)) (mapAsync f as)
 where ap (ContT g) (ContT x) = ContT (apAsync g x)

mapEitherAsync :: forall a b b' eff.
                  (b  -> EitherContT Async a (Eff eff)  b')
               -> [b] -> EitherContT Async a (Eff eff) [b']
mapEitherAsync f [] = return []
mapEitherAsync f (b:bs) = ap ((:) <$> (f b)) (mapEitherAsync f bs)
 where ap (EitherContT g) (EitherContT x) = EitherContT $ \kLeft -> apAsync (g kLeft) (x kLeft)
