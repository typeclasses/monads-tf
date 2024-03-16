# Changelog

## 0.4.0.0

Import improvements from mtl 2.2 and 2.3:

- Add `modifyError` to `Control.Monad.Error.Class`, and re-export from
  `Control.Monad.Except`.
- Make the `MonadCont` instance for `ContT` more polykinded; now, `r` is allowed
  to be of an arbitrary kind `k`, rather than only `Type`.
- Add a generic `liftCallCC` for use with any `MonadTrans`.
- Add `modifyError` to `Control.Monad.Error.Class`
- Add `label` function to `MonadCont`
- Add instances for `Control.Monad.Trans.Writer.CPS` and
  `Control.Monad.Trans.RWS.CPS` from `transformers` 0.5.6 and add
  `Control.Monad.Writer.CPS` and `Control.Monad.RWS.CPS`.
- `Control.Monad.Cont` now re-exports `evalCont` and `evalContT`.
- Add `tryError`, `withError`, `handleError`, and `mapError` to
  `Control.Monad.Error.Class`, and re-export from `Control.Monad.Except`.
- Add instances for `Control.Monad.Trans.Accum` and
  `Control.Monad.Trans.Select`.
- Add `Control.Monad.Accum` for the `MonadAccum` type class, as well as the
  `LiftingAccum` deriving helper.
- Add `Control.Monad.Select` for the `MonadSelect` type class, as well as the
  `LiftingSelect` deriving helper.
- Remove re-exports of `Control.Monad`, `Control.Monad.Fix` and `Data.Monoid` modules
- `Control.Monad.Identity` now re-exports `Control.Monad.Trans.Identity`
- Add a `MonadError Maybe` instance
- Add `liftEither :: MonadError m => Either e a -> m a` to
  `Control.Monad.Except{.Class}`
- Add a `MonadWriter ((,) w)` instance (when built against `base-4.9` or later)
- Provide MINIMAL pragmas for `MonadState`, `MonadWriter`, `MonadReader`
- Added a cyclic definition of `ask` in terms of `reader` for consistency with `get`/`put` vs. `state` and `tell` vs. `writer`

Author: Marcin Serwin

Published by: Chris Martin

Date: 2024-03-15

## 0.3.0.1

Documentation improvements

Author: Ross Paterson

Published by: Chris Martin

Date: 2023-07-10

## 0.3.0.0

Remove deprecated modules:

- `Control.Monad.Error`
- `Control.Monad.Error.Class`
- `Control.Monad.List`

Add support for `transformers-0.6.*`

Published by: Chris Martin

Date: 2023-07-10

## 0.2.1.0

Add `MonadCont`, `MonadReader`, `MonadState`, `MonadRWS`,
`MonadWriter` instances for `ExceptT`.

Published by: Chris Martin

Date: 2023-07-10

## 0.2.0.0

Added new modules `Control.Monad.Except` and
`Control.Monad.Except.Class`

Deprecated modules `Control.Monad.Error` and
`Control.Monad.Error.Class` in favor of the new `Except`
modules, following what `transformers-0.4.0.0` did.

Deprecated module `Control.Monad.List`, following what
`transformers-0.5.3.0` did.

Contributors: Ross Paterson and Chris Martin

Published by: Chris Martin

Date: 2023-07-10

## 0.1.0.3

Published by: Ross Paterson

Date: 2016-06-08

## 0.1.0.2

Published by: Ross Paterson

Date: 2014-04-19

## 0.1.0.1

Published by: Ross Paterson

Date: 2012-09-16

## 0.1.0.0

Published by: Ross Paterson

Date: 2010-03-26

## 0.0.0.1

Published by: Ross Paterson

Date: 2009-03-22

## 0.0.0.0

Published by: Ross Paterson

Date: 2009-01-10
