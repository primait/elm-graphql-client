module Main exposing (main)

import Api
import Auth
import Browser exposing (Document)
import Graphql.Http
import GraphqlClient.Client as GqlClient
import Html exposing (Html, button, div, h1, hr, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model as M exposing (Flags, Model, Msg(..), QueryResult(..), initialModel)


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
    { title = "GraphQL Client"
    , body =
        [ if model.active then
            div
                [ class "container" ]
                [ h1 [] [ text "Elm Graphql Client" ]
                , div [ class "row" ]
                    [ div [ class "col" ] [ p [ class "lead" ] [ text "Graphql client with automatic retry" ] ]
                    ]
                , div [ class "row" ]
                    [ div [ class "col" ] [ button [ class "btn btn-primary", onClick DoQuery ] [ text "execute query" ] ]
                    , div [ class "col" ] [ button [ class "btn btn-primary", onClick DoQueryWithNum ] [ text "execute query with number" ] ]
                    , div [ class "col" ] [ button [ class "btn btn-danger", onClick DoMutationNewNumber ] [ text "mutation to change the number" ] ]
                    , div [ class "col" ] [ button [ class "btn btn-danger", onClick GenerateNewToken ] [ text "invalidate the token" ] ]
                    ]
                , hr [] []
                , case model.result of
                    M.Loading ->
                        div [] [ text "loading..." ]

                    _ ->
                        text ""
                , div [ class "bg-light" ]
                    [ model.result
                        |> mapResult
                            ""
                            (\res ->
                                case res of
                                    Hello { message } ->
                                        "Received: " ++ message

                                    HelloWithNum { message } ->
                                        "Received: " ++ message
                            )
                        |> text
                    ]
                , div [ class "bg-light" ]
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
                        |> Maybe.map ((++) "with number: ")
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
