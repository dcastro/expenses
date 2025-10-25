module Inflection where

-- Pluralize or singularize the input string, depending on the `count` given as the 2nd argument.
foreign import inflect :: String -> Int -> String
