-- |
-- Module      :  Control.Monad.Writer
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Chris Martin <chris@typeclasses.com>
-- Stability   :  experimental
-- Portability :  non-portable (type families)
--
-- The MonadWriter class.
--
-- [Computation type:] Computations which produce an additional value which can be merged.
--
-- [Binding strategy:] Monad values are functions producing an additional value.
-- The bound function is applied to the bound value, and the additional values
-- are merged using '<>'.
--
-- [Useful for:] Logging, tracing.
--
-- [Zero and plus:] None.
--
-- [Example type:] @'Writer' [String] a@
--
--
--      Inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
module Control.Monad.Writer
  ( module Control.Monad.Writer.Lazy,
  )
where

import Control.Monad.Writer.Lazy
