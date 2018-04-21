# GetLineRewrite

A small Haskell library implementing a variant of `getLine` where arbitrary string rewrites may trigger as the user inputs text. This can be useful in implementing a REPL whose syntax includes characters not accessible from a user's keyboard.

## Usage

The module exports one function:

```Haskell
getLineRewrite :: [(String, String)] -> IO String
```

The argument is a list of rewrite rules of the form `(l, r)` indicating that as soon as the user inputs the substring `l`, it should immediately be rewritten to the substring `r`. For example, to rewrite the substring `\lambda` to `λ`, the function should be called as follows:

```Haskell
getLineRewrite [("\\lambda","λ")]
```

![Y Combinator Example](./Y.gif)

## Notes

* As the library relies on ANSI escape sequences to control the cursor position and to erase text, an ANSI-compliant terminal is required to properly display rewrites.
* The ASCII escape character is ignored entirely, and the ASCII delete character results in a backspace. For these reasons (and likely others that I'm unaware of), `getLineRewrite []` doesn't quite behave identically to `getLine`.
* Rewrites can cascade, one being triggered by the result of another. Because of this, and the fact that rewrite rules may by cyclic, such as `[("abc", "xyz"), ("xyz", "abc")]`, there is no general guarantee that a sequence of rewrites will terminate.
* If two rewrite rules overlap, one of the strings to be rewritten being a suffix of the other, only one of the rules will be triggered. The choice is deterministic, based on the ordering of the internal representation, but this should not be relied upon.
