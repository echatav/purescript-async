module Ajax where

import Async
import Control.Monad.Eff

foreign import ajax
  "function ajax(method){\
  \  return function(url){\
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
     String --Method
  -> String --URL
  -> headers
  -> input
  -> (String -> Eff (async :: Async | eff) {}) --Error callback
  -> (output -> Eff (async :: Async | eff) {}) --Success callback
  -> Eff (async :: Async | eff) {}

callProvider :: forall input output eff.
                String --Action
             -> input
             -> (String -> Eff (async :: Async | eff) {}) --Error callback
             -> (output -> Eff (async :: Async | eff) {}) --Success callback
             -> Eff (async :: Async | eff) {}
callProvider act = ajax "POST" ("/api/" ++ act) {"Cache-Control": "no-cache"}

