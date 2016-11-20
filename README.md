# purescript-store

A simple application state store in native PureScript, inspired by [Redux](http://redux.js.org/) (and compatible with the [Redux Dev Tools extension](http://zalmoxisus.github.io/redux-devtools-extension/)).

## Usage

The store is built around the idea of _actions_ (usually an algebraic datatype like `data Action = DoThisThing | DoTheOtherThing`) and an _update_ function. The update function, a pure function, is responsible for updating the application state when you trigger an action using the `dispatch` function. To respond to these state changes, you `subscribe` to the store with an effectful function that updates the environment to reflect the new state.

```purescript
createStore :: ∀ e s a. (Generic a, Generic s) ⇒ (a → s → s) → s → Eff (store :: STORE | e) (Store e a s)
```

You create a store using the `createStore` function, which takes an update function and a starting state. The `Store` you get back has two properties `store.dispatch` and `store.subscribe`, as described above.

```purescript
type Store e a s =
  { subscribe :: (s → Eff e Unit) → Eff (store :: STORE | e) Unit
  , dispatch :: a → Eff (store :: STORE | e) Unit
  }
```

You'll note from looking at `createStore` that your actions and your state must both have `Generic` instances. This is in order to work nicely with the Redux Dev Tools extension, which provides features like being able to export states as JSON files and import them back into a running application. You can usually get the compiler to generate these by using generic deriving, for instance `derive instance genericAction :: Generic Action`.

### Example

A simple working example, where the application state is just an integer, with two actions which increment and decrement it:

```purescript
module Main where

import Data.Generic (class Generic)
import Control.Monad.Eff.Console (log)

import Data.Store (createStore)

type State = Int

seedState :: State
seedState = 0

data Action = Pred | Succ
derive instance genericAction :: Generic Action

update :: Action → State → State
update Pred n = n - 1
update Succ n = n + 1

main = do
  store ← createStore update seedState

  store.subscribe \n → log ("The number is " <> show n)
  -- prints "The number is 0" to the console.

  store.dispatch Succ
  -- prints "The number is 1" to the console.

  store.dispatch Pred
  -- prints "The number is 0" to the console.
```

## Licence

Copyright 2016 Bodil Stokke

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program. If not, see
<http://www.gnu.org/licenses/>.
