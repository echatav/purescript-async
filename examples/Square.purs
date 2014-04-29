module Square where

import Control.Monad.Async
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Data.Array
import Debug.Trace
import Data.Traversable

square :: Number -> Async (Eff (random :: Random, trace :: Trace)) Number
square num = do t <- liftAsync random
                timeout (1000*t)
                liftAsync $ trace $ "Squaring " ++ show num ++ " after " ++ show t ++ " seconds."
                return (num * num)

main :: Eff (random :: Random, trace :: Trace) {}
main = do runAsync (traverse square [1,2,3,4,5,6,7,8,9,10]) $ \results -> do trace "Finished!"
                                                                             print results
          trace "This line happens first!"