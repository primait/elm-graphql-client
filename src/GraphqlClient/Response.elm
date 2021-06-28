module GraphqlClient.Response exposing (Response(..))

import Graphql.Http
import RemoteData exposing (RemoteData)


type Response name decodesTo
    = NotAsked
    | ResponseOk name (RemoteData (Graphql.Http.Error decodesTo) decodesTo)
    | RefreshTokenExpired
    | DefinitiveError name (Graphql.Http.Error decodesTo)
