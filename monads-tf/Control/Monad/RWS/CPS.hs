-- |
-- Module      :  Control.Monad.RWS.Strict
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Chris Martin <chris@typeclasses.com>
-- Stability   :  experimental
-- Portability :  non-portable (type families)
--
-- Strict RWS monad that uses continuation-passing-style to achieve constant
-- space usage.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.
--
-- /Since: monads-tf-0.4.0.0, transformers-0.5.6/
module Control.Monad.RWS.CPS
  ( -- * The RWS monad
    RWS,
    rws,
    runRWS,
    evalRWS,
    execRWS,
    mapRWS,
    withRWS,

    -- * The RWST monad transformer
    RWST,
    runRWST,
    evalRWST,
    execRWST,
    mapRWST,
    withRWST,

    -- * Strict Reader-writer-state monads
    module Control.Monad.RWS.Class,
    module Control.Monad.Trans,
  )
where

import Control.Monad.RWS.Class
import Control.Monad.Trans
import Control.Monad.Trans.RWS.CPS
  ( RWS,
    RWST,
    evalRWS,
    evalRWST,
    execRWS,
    execRWST,
    mapRWS,
    mapRWST,
    runRWS,
    runRWST,
    rws,
    withRWS,
    withRWST,
  )
