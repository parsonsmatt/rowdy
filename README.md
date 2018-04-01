# rowdy

[![Build Status](https://travis-ci.org/parsonsmatt/rowdy.svg?branch=master)](https://travis-ci.org/parsonsmatt/rowdy)
[![Coverage Status](https://coveralls.io/repos/github/parsonsmatt/rowdy/badge.svg?branch=master)](https://coveralls.io/github/parsonsmatt/rowdy?branch=master)

`rowdy` is DSL for defining web application routes.

## Why?

Servant has a cool DSL for routes. It is embedded into the type system, and type
level programming in Haskell isn't nearly as pleasant as value-level
programming. As a result, there can be a lot of complexity with common
combinators, and you must be familiar with advanced type-level techniques to
work with it.

Yesod has a cool DSL for routes. It uses a Template Haskell quasiquoter to parse
a non-Haskell DSL into a data structure, which is then used to generate routes.
Since it isn't Haskell, you don't get good error messages, syntax highlighting,
or the ability to define your own combinators.

`rowdy` is a value level DSL for defining routes. A Template Haskell splice can
then interpret the value at compile-time and provide a Yesod represntation, or a
Servant representation, or whatever else you'd like!

## Future plans

Currently, `rowdy` intends to support Yesod-style routes concretely and
completley. It will be made extensible so that Servant-style routes may be
generated as well, likely in an initially concrete manner (probably just
straight up code duplication). Once support for both platforms is complete, an
abstraction will be picked that will unify the two syntaxes.
