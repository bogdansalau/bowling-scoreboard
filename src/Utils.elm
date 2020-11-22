module Utils exposing (..)

removeHead: List a -> List a
removeHead list =
  case list of
    [] -> []
    _::xs -> xs

dropWhile : (a -> Bool) -> List a -> List a
dropWhile p l =
  case l of
    [] -> []
    x::xs -> if p x
               then dropWhile p xs
               else x::xs

