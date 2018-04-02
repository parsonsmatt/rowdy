# `rowdy-yesod`

An implementation of the `rowdy` web route DSL for the Yesod web framework.
Check the [GitHub repo](https://www.github.com/parsonsmatt/rowdy) for more
information and examples.

```haskell
routes = do
    get "RootR"
    "users" // do
        resource "UserIndexR" [get, post]
        capture @Int // resource "UserR" [get, put]
    "admin" // "Admin" /: do
        get "PanelR" ! "admin" ! "cool"
        post "PanelR" ! "admin"
    "other-attr" // "safe" /! do
        get "SafeR"
        put "SafeR"
```
