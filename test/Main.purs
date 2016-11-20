module Test.Main where

import Prelude
import Test.Unit.Assert as Assert
import Control.Monad.Aff (makeAff, Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Generic (class Generic)
import Data.Store (Store, STORE, createStore)
import Data.Tuple (Tuple(Tuple))
import Test.Unit (test)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

data Action = SuccLeft | PredLeft | SuccRight | PredRight
derive instance genericAction :: Generic Action

initialState :: Tuple Int Int
initialState = Tuple 0 0

update :: Action → Tuple Int Int → Tuple Int Int
update SuccLeft (Tuple left right) = Tuple (left + 1) right
update PredLeft (Tuple left right) = Tuple (left - 1) right
update SuccRight (Tuple left right) = Tuple left (right + 1)
update PredRight (Tuple left right) = Tuple left (right - 1)

onState :: ∀ e a s. Store e a s → Aff (store :: STORE | e) s
onState store = unsafeCoerceAff $ makeAff \_ resolve →
  unsafeCoerceEff $ store.subscribe resolve

main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, store :: STORE | e) Unit
main = runTest do
  test "dispatch and subscribe" do
    store ← liftEff $ createStore update initialState
    liftEff $ store.dispatch PredLeft
    liftEff $ store.dispatch SuccRight
    onState store >>= Assert.equal (Tuple (-1) 1)
