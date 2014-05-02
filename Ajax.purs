module Ajax where

import Async
import Control.Monad.Eff

foreign import ajax
  "function ajax(url){\
  \  return function(method){\
  \    return function(headers){\
  \      return function(input){\
  \        return function(kErr){\
  \          return function(KSuc){\
  \            return function(){\
  \              jQuery.ajax({ url: url\
  \                          , method: method\
  \                          , headers: headers\
  \                          , data: input\
  \                          , error: function(jqXHR,err){kErr(err)}\
  \                          , success: kSuc\
  \                          });\
  \};};};};};};}"
  :: forall headers input output eff.
     String --URL
  -> String --Method
  -> headers
  -> input
  -> (String -> Eff (async :: Async | eff) {}) --Error callback
  -> (output -> Eff (async :: Async | eff) {}) --Success callback
  -> Eff (async :: Async | eff) {}