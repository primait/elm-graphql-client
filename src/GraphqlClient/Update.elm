module GraphqlClient.Update exposing (update)

{-| Component update

@docs update

-}

import Graphql.Http exposing (HttpError(..), RawError(..))
import GraphqlClient.Model as M exposing (Model, Msg(..))
import GraphqlClient.Response exposing (Response(..))
import RemoteData as RD


{-| Run the update for the component
-}
update :
    Msg name decodesTo
    -> Model name decodesTo msg
    -> ( Model name decodesTo msg, Cmd (Msg name decodesTo), Response name decodesTo )
update msg model =
    case msg of
        FirstAttemptFinished queryId queryName remoteData ->
            let
                isTokenError : Bool
                isTokenError =
                    case remoteData of
                        RD.Failure (HttpError (BadStatus metadata _)) ->
                            metadata.statusCode == 401

                        RD.Failure (GraphqlError _ graphqlErrors) ->
                            M.getAuthenticationErrorIdentifier model graphqlErrors

                        _ ->
                            False

                newModel : Model name decodesTo msg
                newModel =
                    case remoteData of
                        RD.NotAsked ->
                            model

                        RD.Loading ->
                            model

                        RD.Failure _ ->
                            if isTokenError then
                                model

                            else
                                M.removeQueryFromPipeline queryId model

                        RD.Success _ ->
                            M.removeQueryFromPipeline queryId model

                response : Response name decodesTo
                response =
                    case remoteData of
                        RD.NotAsked ->
                            NotAsked

                        RD.Loading ->
                            NotAsked

                        RD.Failure error ->
                            if isTokenError then
                                RefreshTokenExpired

                            else
                                DefinitiveError queryName error

                        RD.Success _ ->
                            ResponseOk queryName remoteData
            in
            ( newModel, Cmd.none, response )

        SecondAttemptFinished _ queryName remoteData ->
            ( model
            , Cmd.none
            , case remoteData of
                RD.NotAsked ->
                    NotAsked

                RD.Loading ->
                    NotAsked

                RD.Failure error ->
                    DefinitiveError queryName error

                RD.Success _ ->
                    ResponseOk queryName remoteData
            )
