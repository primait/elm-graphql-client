module Main exposing (main)

import Api
import Auth
import Browser exposing (Document)
import Graphql.Http
import Html exposing (Html, button, div, hr, text)
import Html.Events exposing (onClick)
import Model as M exposing (Flags, Model, Msg(..), QueryResult(..), initialModel)
import PrimaElm.Lib.GqlClient.GqlClient as GqlClient


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Graphql.Http.queryRequest "http://localhost:8002" Auth.fetchTokenQuery
        |> Graphql.Http.send (ReceivedToken True)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedToken updateComponent maybeToken ->
            if updateComponent then
                maybeToken
                    |> Result.toMaybe
                    |> Maybe.map
                        (\token ->
                            let
                                auth : List ( String, String )
                                auth =
                                    [ ( "authorization", token ) ]

                                newModel =
                                    { model | active = True, gqlClientAuthentication = auth }
                            in
                            GqlClient.authenticationChanged
                                model.gqlClientModel
                                newModel.gqlClientAuthentication
                                M.setGqlClientModel
                                newModel
                        )
                    |> Maybe.withDefault ( model, Cmd.none )

            else
                ( model, Cmd.none )

        DoQuery ->
            let
                ( gqlClientModel, gqlCommand ) =
                    GqlClient.runQuery M.All model.gqlClientAuthentication model.gqlClientModel Api.helloQuery
            in
            ( { model | result = M.Loading, gqlClientModel = gqlClientModel }
            , Cmd.map GqlClientMsg gqlCommand
            )

        DoQueryWithNum ->
            let
                ( gqlClientModel, gqlCommand ) =
                    GqlClient.runQuery M.All model.gqlClientAuthentication model.gqlClientModel Api.helloQueryNum
            in
            ( { model | result = M.Loading, gqlClientModel = gqlClientModel }
            , Cmd.map GqlClientMsg gqlCommand
            )

        DoMutationNewNumber ->
            let
                ( gqlClientModel, gqlCommand ) =
                    GqlClient.runMutation M.All model.gqlClientAuthentication model.gqlClientModel Api.newNumberMutation
            in
            ( { model | result = M.Loading, gqlClientModel = gqlClientModel }
            , Cmd.map GqlClientMsg gqlCommand
            )

        GenerateNewToken ->
            ( model
            , Graphql.Http.mutationRequest "http://localhost:8002" Auth.generateNewToken
                |> Graphql.Http.send (ReceivedToken False)
            )

        GqlClientMsg gqlClientMsg ->
            GqlClient.manageUpdate
                gqlClientMsg
                model.gqlClientModel
                (Graphql.Http.send (ReceivedToken True) <| Graphql.Http.queryRequest "http://localhost:8002" Auth.fetchTokenQuery)
                M.handleQueryResponse
                M.handleQueryError
                M.setGqlClientModel
                model


mapResult : a -> (QueryResult -> a) -> M.Res -> a
mapResult default func res =
    case res of
        M.NotAsked ->
            default

        M.Loading ->
            default

        M.Ok queryResult ->
            func queryResult


view : Model -> Document Msg
view model =
    { title = "PrimaElm - GraphQL Client"
    , body =
        [ if model.active then
            div
                []
                [ button [ onClick DoQuery ] [ text "execute query" ]
                , button [ onClick DoQueryWithNum ] [ text "execute query with num" ]
                , button [ onClick DoMutationNewNumber ] [ text "execute mutation new number" ]
                , button [ onClick GenerateNewToken ] [ text "generate new token" ]
                , hr [] []
                , case model.result of
                    M.Loading ->
                        div [] [ text "loading..." ]

                    _ ->
                        text ""
                , div []
                    [ model.result
                        |> mapResult
                            ""
                            (\res ->
                                case res of
                                    Hello { message } ->
                                        message

                                    HelloWithNum { message } ->
                                        message
                            )
                        |> text
                    ]
                , div []
                    [ model.result
                        |> mapResult
                            Nothing
                            (\res ->
                                case res of
                                    Hello _ ->
                                        Nothing

                                    HelloWithNum { num } ->
                                        Just num
                            )
                        |> Maybe.map String.fromInt
                        |> Maybe.withDefault ""
                        |> text
                    ]
                , div [] [ model.error |> Maybe.withDefault "" |> text ]
                ]

          else
            div [] [ text "loading..." ]
        ]
    }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
