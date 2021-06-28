module GraphqlClient.Utils exposing (flip)


{-| Flip the order of the first two arguments of a function.
-}


flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b
