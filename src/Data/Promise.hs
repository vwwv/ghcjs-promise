{-# LANGUAGE JavaScriptFFI
           , OverloadedStrings 
           , CPP
           #-}

module Data.Promise( Promise()
                   , await
                   , promise
                   ) where

import GHCJS.Marshal
import GHCJS.Types
import GHCJS.Foreign
import Control.Exception
import Control.Concurrent




newtype Promise = Promise {fromPromise :: JSVal}


-- This works because the [algorithm](http://www.ecma-international.org/ecma-262/6.0/#sec-promise.resolve) 
-- explicitly demands that Promise.resolve must return the exact object passed in if and only if 
-- it is a promise by the definition of the spec.
-- (from stackoverflow http://stackoverflow.com/questions/27746304/how-do-i-tell-if-an-object-is-a-promise)
instance FromJSVal Promise where
      fromJSVal x = do is_promise <- js_check_if_promise x
                       if is_promise
                        then return . Just $ Promise x
                        else return Nothing

instance ToJSVal Promise where
      toJSVal   =  return . fromPromise


await   :: Promise -> IO (Either JSVal JSVal)
await (Promise jsval) = do result <- js_await     jsval
                           x      <- js_attribute "result" result
                           ok     <- isTruthy <$> js_attribute "ok" result
                           if ok
                            then return (Right x)
                            else return (Left  x)


promise :: IO JSVal -> IO Promise
promise action = do ref     <- js_random_uuid
                    promise <- js_set_promise ref
                    forkIO $ do val_ <- try action
                                case val_ of
                                  Right x -> js_resolve ref x
                                  Left  x -> js_reject  ref =<< toJSVal (show (x::SomeException))
                    js_free_handles ref 

                    return $ Promise promise
-----------------------------------------------------------------------
-----------------------------------------------------------------------


-- TODO: check it always return True!
foreign import javascript safe
    "delete (window or global)[$1];"
    js_free_handles :: JSString -> IO ()

foreign import javascript safe
    "                                                                       \
      function S4() {                                                       \
          return (((1+Math.random())*0x10000)|0).toString(16).substring(1); \
      }                                                                     \
      $r = ( S4() + S4()                                                    \
           + '-'  + S4()                                                    \
           + '-'  + '4' + S4().substr(0,3)                                  \
           + '-'  + S4()                                                    \
           + '-'  + S4() + S4() + S4()                                      \
           ).toLowerCase();                                                 \
    "
    js_random_uuid :: IO JSString 

 

foreign import javascript safe
    "(window or global)[$1]['resolve']($2);"
    js_resolve      :: JSString -> JSVal -> IO ()

foreign import javascript safe
    "(window or global)[$1]['reject'](new Error($2));"
    js_reject       :: JSString -> JSVal -> IO ()



foreign import javascript safe 
    "Promise.resolve($1) == $1"  
    js_check_if_promise :: JSVal -> IO Bool

foreign import javascript safe 
    "$2[$1]"
    js_attribute :: JSString -> JSVal -> IO JSVal

-- TODO, check how it affects the "this" reference.
foreign import javascript safe
    "                                                                         \
      new Promise( function(resolve, reject){                                 \
                       (window or global)[$1] = { resolve : resolve           \
                                                , reject  : reject            \
                                                }                             \
                   }                                                          \
                 )                                                            \
    "
    js_set_promise  :: JSString -> IO JSVal



foreign import javascript interruptible
   " console.log('setting capture callback');                                 \
    try{                                                                      \
        ($1.capture().then(function(x){$c(x)}))['catch'](function(x){$c(x)}); \
     } catch(err) {                                                           \
         console.log('got into the catch') ;                                  \
         console.log(err);                                                    \
    }                                                                         \
    console.log('callback set');                                              \
    "   
   js_take_picture :: JSVal -> IO JSVal
   

foreign import javascript interruptible
    " $1.then(     function(x){                                               \
                     $c({ result : x, ok : true})                             \
                   }                                                          \
             );                                                               \
                                                                              \
      $1['catch']( function(x){                                               \
                     $c({ result : x, ok : false})                            \
                   }                                                          \
                );                                                            \
    "
    js_await     :: JSVal -> IO JSVal







