module Square where

import Async
import Control.Monad.Eff
import Control.Monad.Trans
import Control.Monad.Cont.Trans
import Control.Monad.Eff.Random
import Debug.Trace

square :: Number -> ContT {} (Eff (random :: Random, trace :: Trace, async :: Async)) Number
square num = do t <- lift random
                timeoutCont (1000*t)
                lift $ trace $ "Squaring " ++ show num ++ " after " ++ show t ++ " seconds."
                return (num * num)

main :: Eff (random :: Random, trace :: Trace, async :: Async) {}
main = do runContT (mapAsync square [1,2,3,4,5,6,7,8,9,10]) $ \results -> do trace "Finished!"
                                                                             print results
          trace "This line happens first!"