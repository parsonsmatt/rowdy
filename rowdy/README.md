# `rowdy`

The core routing DSL for `rowdy`.
Check the [GitHub repo](https://www.github.com/parsonsmatt/rowdy) for more
information and examples.

```haskell
-- Yesod-style:
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
