-- |
-- Module      :  Control.Monad.State
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (type families)
--
-- = State monads
--
-- [Computation type:] Computation that has access to a modifiable state.
--
-- [Binding strategy:] Monad values are functions that receive a state and
-- return the modified state along with the value. The bound function is applied
-- to the modified state returned by the bound value.
--
-- [Useful for:] Algorithms that require modifiable state.
--
-- [Zero and plus:] None.
--
-- [Example type:] @'State' s a@
module Control.Monad.State
  ( module Control.Monad.State.Lazy,
  )
where

import Control.Monad.State.Lazy
