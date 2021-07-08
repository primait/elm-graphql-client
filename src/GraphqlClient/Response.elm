module GraphqlClient.Response exposing (Response(..))

{-| module for the Response type

@docs Response

-}

import Graphql.Http
import RemoteData exposing (RemoteData)


{-| The response type
-}
type Response name decodesTo
    = NotAsked
    | ResponseOk name (RemoteData (Graphql.Http.Error decodesTo) decodesTo)
    | RefreshTokenExpired
    | DefinitiveError name (Graphql.Http.Error decodesTo)
