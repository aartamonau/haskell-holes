---
title: haskell-holes
---

## Rationale

**Clojure** programming language has a prominent reader form which enables you
to define anonymous functions in a slightly less verbose way. For instance,
instead of writing `(fn [x y z] (+ x (/ z y)))` you can just express it as
`#(+ %1 (/ %3 %2))`. In **Haskell** we sometimes use point-free style to
achieve similar goals. Though the result can be disappointing as regards the
readability of the result (no doubt that it heavily depends on the function to
be defined in such way). Another issue I sometimes have is the need to invent
meaningful names for the parameters which is not always possible. Such a
shortcut form allows to use just a number to identify parameters. Though I can
define lambda as `\_1 _2 _3 -> _1 + (_3 / _2)` I don't find it appropriate. It
feels much better psychologically when it's not you who makes hacky (or just
not very beautiful) things but some automated machinery. That's why this
package exists.

## Usage

**Haskell-holes** package is implemented as a Template Haskell quasi-quoter
(more on implementation details in the next section). So the meaningless
example from the previous section can be rewritten using **haskell-holes** as
follows:

~~~~~~~~{.haskell}
import Language.Haskell.Holes
>>> :t [$holes| %1 + (%3 / %2) |]
[$holes| %1 + (%3 / %2) |] :: (Fractional a) => a -> a -> a -> a
~~~~~~~~

Up to 9 numbered arguments are supported (`%1` through `%9`). Though there is
no way to say "all arguments altogether" (in **Clojure** it's just `%`). The
number of arguments which the anonymous function will take is determined by
the maximum argument number that is used in its body. Another syntactic
limitation: to avoid ambiguity `%` should be surrounded by spaces if it's used
in its usual meaning.

Using **haskell-holes** standard *map* function can be defined (rather
cryptically) as follows:

~~~~~~~~{.haskell}
import Data.Function
import Language.Haskell.Holes

>>> let map = fix $ [holes| if null %3 then [] else %2 (head %3) : %1 %2 (tail %3) |]
>>> :t map
map :: (a -> t) -> [a] -> [t]
~~~~~~~~

## Reflections

This is my first experience with **Template Haskell** and quasi-quoters. An
overall impression is positive: it's nice to know that if you are not
satisfied with expressiveness of **Haskell** you can use **TH** as a last
resort. Though it was quite disappointing to realize that in order to *extend*
syntax slightly you need to implement full-featured **Haskell** parser. So my
big thanks goes to
[haskell-src-exts](http://hackage.haskell.org/package/haskell-src-exts) and
[haskell-src-meta](http://hackage.haskell.org/package/haskell-src-meta)
projects which can parse source code in **Haskell** into a **TH** abstract
syntax. But I think that many people will agree with me that it would be nice to
see this functionality eventually in **GHC** out of the box.
