module GraphqlClient.Utils exposing (find, flip, isJust)

{-| Flip the order of the first two arguments of a function.
-}


flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b


isJust : Maybe a -> Bool
isJust maybeValue =
    case maybeValue of
        Just a ->
            True

        Nothing ->
            False


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.

    find (\num -> num > 5) [ 2, 4, 6, 8 ]
    --> Just 6

-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest
