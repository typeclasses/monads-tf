# `monads-tf`

`monads-tf` is a collection of monad classes, extending the `transformers`
package, using indexed type families for generic lifting of monadic
actions. The purpose of this package is similar to the MTL package
which also provides monad classes but uses functional dependencies.

## Structure

Transformers in `monads-tf` are divided into classes and data types. Classes
define the monadic operations of transformers. Data types, generally
from the `transformers` package, implement transformers, and `monads-tf`
provides instances for all the transformer type classes.

`monads-tf` and `transformers` use a common module, data type, and function
naming scheme. As an example, let's imagine we have a transformer `Foo`.

In the `Control.Monad.Foo` module, we'd find:

- A type class `MonadFoo` with the transformer operations.
- A data type `FooT` with instances for all monad transformer classes.
- Functions to run the transformed computation, e.g. `runFooT`. For
  the actual transformers, there are usually a number of useful runner
  functions.

### Lifting

When using monad transformers, you often need to "lift" a monadic
action into your transformed monadic action. This is done using the
`lift` function from `MonadTrans` in the `Control.Monad.Trans.Class`
module:

```haskell
lift :: (Monad m, MonadTrans t) => m a -> t m a
```

The action `m a` is lifted into the transformer action `t m a`.

As an example, here we lift an action of type `IO a` into an action of
type `ExceptT MyError IO a`:

```haskell
data MyError = EmptyLine

mightFail :: ExceptT MyError IO ()
mightFail = do
  l <- lift getLine
  when (null l) (throwError EmptyLine)
```

### Transformers

The following outlines the available monad classes and transformers in
`monads-tf` and `transformers`. For more details, and the corresponding
documentation of the `monads-tf` version you are using, see [the
documentation on Hackage](https://hackage.haskell.org/package/monads-tf).

#### `Control.Monad.Cont`

The Continuation monad transformer adds the ability to use
[continuation-passing style
(CPS)](https://en.wikipedia.org/wiki/Continuation-passing_style)
in a monadic computation. Continuations can be used to manipulate
the control flow of a program, e.g. early exit, error handling, or
suspending a computation.

- Class: `Control.Monad.Cont.Class.MonadCont`
- Transformer: `Control.Monad.Cont.ContT`

#### `Control.Monad.Except`

The Except monad transformer adds the ability to fail with an
error in a monadic computation.

- Class: `Control.Monad.Error.Class.MonadError`
- Transformer: `Control.Monad.Except.ExceptT`

#### `Control.Monad.Identity`

The Identity monad transformer does not add any abilities to a
monad. It simply applies the bound function to its inner monad
without any modification.

- Transformer: `Control.Monad.Trans.Identity.IdentityT` (in the `transformers` package)
- Identity functor and monad: `Data.Functor.Identity.Identity` (in the `base` package)

#### `Control.Monad.RWS`

A convenient transformer that combines the Reader, Writer, and
State monad transformers.

- Lazy transformer: `Control.Monad.RWS.Lazy.RWST` (which is the default, exported by `Control.Monad.RWS`)
- Strict transformer: `Control.Monad.RWS.Strict.RWST`

#### `Control.Monad.Reader`

The Reader monad transformer represents a computation which can
read values from an environment.

- Class: `Control.Monad.Reader.Class.MonadReader`
- Transformer: `Control.Monad.Reader.ReaderT`

#### `Control.Monad.State`

The State monad transformer represents a computation which can
read and write internal state values. If you only need to _read_
values, you might want to use [Reader](#controlmonadreader) instead.

- Class: `Control.Monad.State.Class.MonadState`
- Lazy transformer: `Control.Monad.State.Lazy.StateT` (the default, exported by `Control.Monad.State`)
- Strict transformer: `Control.Monad.State.Strict.StateT`

#### `Control.Monad.Writer`

The Writer monad transformer represents a computation that can
produce a stream of data in addition to the computed values. This
can be used to collect values in some data structure with a
`Monoid` instance. This can be used for things like logging and
accumulating values throughout a computation.

- Class: `Control.Monad.Writer.Class.MonadWriter`
- Lazy transformers: `Control.Monad.Writer.Lazy.WriterT`
- Strict transformers: `Control.Monad.Writer.Strict.WriterT`

#### `Control.Monad.Accum`

The `Accum` monad transformer represents a computation which
manages append-only state, or a writer that can read all
previous inputs. It binds a function to a monadic value by
lazily accumulating subcomputations via `(<>)`. For more general
access, use [State](#controlmonadstate) instead.

- Class: `Control.Monad.Accum`
- Transformer: `Control.Monad.Trans.Accum.AccumT`

#### `Control.Monad.Select`

The `Select` monad transformer represents a computation which
can do backtracking search using a 'ranked' evaluation strategy.
Binding a function to a monad value chains together evaluation
strategies in the sense that the results of previous strategies
may influence subsequent rank and evaluation strategies in
subcomputations.

- Class: `Control.Monad.Select`
- Transformer: `Control.Monad.Trans.Select.SelectT`

## Acknowledgements

The code and documentation (including this readme file) is derived
from the [MTL library](https://github.com/haskell/mtl). The license
of the MTL code is the same as of this library and can be found in
the file [LICENSE](./LICENSE). The authors of particular Haskell code
can be found in the `Copyright` field at the beginning of each `.hs` file.

## Resources

- [`monads-tf` on Hackage](http://hackage.haskell.org/package/monads-tf)
- The [Monad Transformers](https://en.wikibooks.org/wiki/Haskell/Monad_transformers)
  chapter in Wikibooks "Haskell" book.
- References:
  - This package is inspired by the paper _Functional Programming
    with Overloading and Higher-Order Polymorphism_, by Mark P
    Jones, in _Advanced School of Functional Programming_, 1995
    (<https://web.cecs.pdx.edu/~mpj/pubs/springschool.html>).
  - [`mtl` on Hackage](http://hackage.haskell.org/package/mtl)
