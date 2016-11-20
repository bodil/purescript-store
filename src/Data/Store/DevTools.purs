module Data.Store.DevTools
  ( connect
  , subscribe
  , send
  , init
  , Dispatch
  , DevTools
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Data.Array (fromFoldable)
import Data.Either (Either(Left, Right))
import Data.Foreign (MultipleErrors, Foreign)
import Data.Foreign.Generic (defaultOptions, readGeneric, toForeignGeneric)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (toNullable, Nullable)
import Data.Store.Types (STORE)

type Dispatch e a s =
  { reset :: Eff e Unit
  , commit :: Eff e Unit
  , rollback :: s → Eff e Unit
  , setState :: s → Eff e Unit
  , dispatch :: a → Eff e Unit
  }

foreign import data DevTools :: * → * → *

type ParseResult a = { error :: Nullable (Array String), result :: Nullable a }

toParseResultObj :: ∀ a. Either MultipleErrors a → ParseResult a
toParseResultObj (Right a) =
  { error: toNullable Nothing
  , result: toNullable $ Just a
  }
toParseResultObj (Left errors) =
  { error: toNullable $ Just $ show <$> fromFoldable errors
  , result: toNullable Nothing
  }

connect :: ∀ e a s. (Generic a, Generic s) ⇒ Eff (store :: STORE | e) (DevTools a s)
connect = connectP (toForeignGeneric defaultOptions) (readGeneric defaultOptions >>> runExcept >>> toParseResultObj) (toForeignGeneric defaultOptions) (readGeneric defaultOptions >>> runExcept >>> toParseResultObj)

foreign import connectP :: ∀ e a s. (a → Foreign) → (Foreign → ParseResult a) → (s → Foreign) → (Foreign → ParseResult s) → Eff (store :: STORE | e) (DevTools a s)
foreign import subscribe :: ∀ e a s. DevTools a s → Dispatch (store :: STORE | e) a s → Eff (store :: STORE | e) Unit
foreign import send :: ∀ e a s. DevTools a s → a → s → Eff (store :: STORE | e) Unit
foreign import init :: ∀ e a s. DevTools a s → s → Eff (store :: STORE | e) Unit
