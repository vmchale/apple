# apple

  * Proper type of `frange`
  * Display integer types with index in REPL
  * Don't crash on some type errors
  * Allow tuple types in annotations

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
