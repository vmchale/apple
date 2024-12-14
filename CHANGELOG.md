# apple

  * Proper type of `frange`
  * Display integer types with index in REPL
  * Allow tuples in type annotations
  * Parsing infix expressions of min/max will no longer crash
  * REPL shows constraints in types
  * Don't crash when on `:inspect`ing a scalar
  * Booleans are allowed as arguments to functions
  * Implement `IsEq` for booleans &c.
  * Prettier-print expressions in error messages
  * Parse errors report with Happy's `explist`
  * Functions report that they are not a member of typeclasses
  * Tie up various cases in typechecker (no longer bail out/crash)
  * Builtins (`frange`, `irange`, etc.) in REPL compleitions 
  * No longer segfault when `irange` is specified backwards
  * Store functions in REPL
  * Fix bug in parsing curried binary operators
  * Remove `re:` in favor of infix 〃

# 0.3.0.0

  * Display dimension information in REPL results
  * Fix bugs
  * Rotate (⊖), head, last more polymorphic (work on arrays).
  * More cases in the mid-end
  * Lifting constants out of loops works again
  * REPL no longer fails when being unable to monomorphize unused binds
  * Type signatures behave like an infix operator; more sensible
  * Building library no longer requires jacinda; prefer ripgrep/awk
  * Add function composition, `∴`
  * Implement equality on arrays
  * Add unicode fraction literals ⅚ &c.

## 0.2.0.0

  * Add `:delete` to REPL
  * Some cases no longer crash the compiler
  * Fix `free` insertion in branches
  * Implement filter (`§`) and 'indices of' (`⩪`)
  * Add `{:?` (tailM) and `}:?` (initM)
  * Existentials are handled better

## 0.1.0.0

Initial release
