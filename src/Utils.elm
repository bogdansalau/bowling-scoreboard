module Utils exposing (..)

removeHead: List a -> List a
removeHead list =
  case list of
    [] -> []
    _::xs -> xs
