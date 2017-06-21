module Data.JSVal.Typed.Promise (
  Promise,
  await,
  awaitMaybe,
  AwaitError(..)
) where

import Protolude (maybeToRight)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Either.Combinators (rightToMaybe, mapLeft)
import qualified Data.JSVal.Promise as P

import GHCJS.Types
import GHCJS.Marshal

-- | A Typed Promise is tagged with the expected type of the eventual value.
--   We don't export the constructor or field accessor, so the user must provide a
--   `FromJSVal` instance when awaiting the value.
--
--   This allows an inline Javascript function to return @Promise a@ and enforce
--   type safety in the return. @Promise@ is a newtype around @JSVal@ so is valid
--   to be passed through Javascript FFI.
newtype Promise a = Promise JSVal

instance IsJSVal (Promise a)

data AwaitError = InvalidType
                | PromiseErr JSVal

-- | Await a value and attempt to cast into the correct return type.
await :: FromJSVal a => Promise a -> IO (Either AwaitError a)
await (Promise js) = runExceptT $ do
  untyped <- fromJSVal' js
  p <- ExceptT $ mapLeft PromiseErr <$> P.await untyped
  fromJSVal' p

-- | Suppress error messages and just return a `Maybe` for convenience.
awaitMaybe :: FromJSVal a => Promise a -> IO (Maybe a)
awaitMaybe p = rightToMaybe <$> await p

fromJSVal' :: FromJSVal a => JSVal -> ExceptT AwaitError IO a
fromJSVal' j = ExceptT $ maybeToRight InvalidType <$> fromJSVal j
