# rowdy

[![Build Status](https://travis-ci.org/parsonsmatt/rowdy.svg?branch=master)](https://travis-ci.org/parsonsmatt/rowdy)

`rowdy` is DSL for defining web application routes. Check out [the examples](examples/)!

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
then interpret the value at compile-time and provide a Yesod representation, or a
Servant representation, or whatever else you'd like!

## Future plans

Currently, `rowdy` supports Yesod-style routes concretely and completely. The
Servant support is limited and not ready for publish yet -- if you have ideas
for providing an extensible design for this, please let me know!
