# An Emacs Lisp Complex Number Library

The "cplx" package represents complex numbers as a pair of floats in a
cons cell.

Except for the constructors, `cplx` and `cplx-const`, and type
predicate, `cplx-p`, each function accepts only complex numbers,
returning either a complex number or scalar as appropriate. To pass a
scalar, represent it as a complex number with a zero imaginary
component, e.g. `(cplx 1.0 0.0)`.

To efficiently embed complex constants in within a compiled function's
constants vector, use the `cplx-const` macro. The alternative would be
to construct them repeatedly at run time on demand, or continually
fish them from a global variable.

```el
(let ((i (cplx-const 0.0 1.0)))
   ...)
```

Constructor, predicate, and accessors:

* `(cplx real imag)`
* `(cplx-const real imag)`
* `(cplx-p object)`
* `(cplx-real c)`
* `(cplx-imag c)`

Basic operators:

* `(cplx-+ a b)`
* `(cplx-- a b)`
* `(cplx-* a b)`
* `(cplx-/ a b)`

Unary functions:

* `(cplx-abs c)`
* `(cplx-arg c)`
* `(cplx-conj c)`
* `(cplx-proj c)`
* `(cplx-sqrt c)`
* `(cplx-sin c)`
* `(cplx-cos c)`
* `(cplx-tan c)`
* `(cplx-log c)`
* `(cplx-asin c)`
* `(cplx-acos c)`
* `(cplx-atan c)`

Unimplemented (wish list):

* `(cplx-exp c)`
* `(cplx-expt a b)`

## Philosophy

This package is optimized for a byte code compiler that does not yet
exist. Every function in this package is defined as inlined (e.g.
`defsubst`), and there are no versions of any of these functions that
accept plain scalars. With the current Emacs byte code compiler,
functions that call many complex number functions will compile to much
larger byte code objects than necessary and will perform a little
slower than strictly necessary.

In practice, complex number functions are not called in isolation.
Many complex operations are typically performed at once on multiple
values. By inlining every complex number function call, the byte code
compiler can *theoretically* optimize across all these calls. Using
escape analysis — especially with lexical scope — the compiler could
elide most intermediate complex number objects, eliminating
unnecessary consing. Constants could be folded, particularly in the
case of `cplx-const`. Dead code could be removed.

None of this requires a "[sufficiently smart compiler][opt]". These
are straightforward, routine compiler optimizations. Unfortunately the
current Emacs byte code compiler just doesn't do any of these things.
This package is optimistic, assuming that one day it will.

Also unfortunately, multiply-by-zero cannot generally be optimized for
floating point values due to the presence of NaN, infinity, and
negative zero. This prevents some scalar constant operations from
being optimized. For example, while this would be great, this would
*not* be a valid optimization by the byte compiler:

```el
;; before;
(cplx-* (cplx-const 2.0) c)

;; after:
(cons (* 2.0 (car c)) (* 2.0 (cdr c)))
```

A nice side effect of all the function inlining is that this package
is like a macro-only package, only required at compile time. The
`require` for this package can be wrapped in an `eval-when-compile`.


[opt]: http://wiki.c2.com/?SufficientlySmartCompiler
