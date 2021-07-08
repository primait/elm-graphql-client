module GraphqlClient.Model exposing
    ( resetPipeline
    , removeQueryFromPipeline
    , Authentication
    , Model
    , Msg(..)
    , PipelineElement(..)
    , QueryId
    , addElementToPipeline
    , getAuthenticationErrorIdentifier
    , getElementsInPipeline
    , getMsgLifter
    , getPipelineElementName
    , getUrl
    , initialize
    )

{-| The Model

@docs resetPipeline
@docs removeQueryFromPipeline
@docs Authentication
@docs Model
@docs Msg
@docs PipelineElement
@docs QueryId
@docs addElementToPipeline
@docs getAuthenticationErrorIdentifier
@docs getElementsInPipeline
@docs getMsgLifter
@docs getPipelineElementName
@docs getUrl
@docs initialize

-}

import Dict
import Graphql.Http
import Graphql.Http.GraphqlError exposing (GraphqlError)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
import GraphqlClient.Config as Config exposing (Config)
import GraphqlClient.Utils exposing (find, flip, isJust)
import Json.Decode exposing (Decoder, at, bool, decodeValue)
import RemoteData exposing (RemoteData)


{-| Header name
-}
type alias HeaderName =
    String


{-| Header value
-}
type alias HeaderValue =
    String


{-| Represent an authentication header
-}
type alias Authentication =
    ( HeaderName, HeaderValue )


{-| Query id
-}
type alias QueryId =
    Int


{-| A pipeline element
-}
type PipelineElement name decodesTo
    = Query name (SelectionSet decodesTo RootQuery)
    | Mutation name (SelectionSet decodesTo RootMutation)


{-| The component model
-}
type Model name decodesTo msg
    = Model
        { config : Config
        , pipeline : List ( QueryId, PipelineElement name decodesTo )
        , msgLifter : Msg name decodesTo -> msg
        , authenticationErrorIdentifier : List GraphqlError -> Bool
        }


{-| Initialize the model
-}
initialize : String -> (Msg name decodesTo -> msg) -> Model name decodesTo msg
initialize url msgLifter =
    Model
        { config = Config.initialize url
        , pipeline = []
        , msgLifter = msgLifter
        , authenticationErrorIdentifier = defaultAuthenticationErrorIdentifier
        }


{-| a default authentication error identifier
It reads inside the gql response for the key "extensions", and look for a key named "missing\_token" with boolean value of true
-}
defaultAuthenticationErrorIdentifier : List GraphqlError -> Bool
defaultAuthenticationErrorIdentifier =
    let
        errorDecoder : Decoder Bool
        errorDecoder =
            at [ "missing_token" ] bool
    in
    List.foldl (\{ details } acc -> details :: acc) []
        >> List.filterMap (Dict.get "extensions")
        >> List.filterMap (Result.toMaybe << decodeValue errorDecoder)
        >> find identity
        >> isJust


{-| The Msg
-}
type Msg name decodesTo
    = FirstAttemptFinished QueryId name (RemoteData (Graphql.Http.Error decodesTo) decodesTo)
    | SecondAttemptFinished QueryId name (RemoteData (Graphql.Http.Error decodesTo) decodesTo)


{-| Gets the config
-}
getConfig : Model name decodesTo msg -> Config
getConfig (Model { config }) =
    config


{-| Gets the client url
-}
getUrl : Model name decodesTo msg -> String
getUrl =
    Config.getUrl << getConfig


{-| Gets pipeline elements
-}
getElementsInPipeline : Model name decodesTo msg -> List ( QueryId, PipelineElement name decodesTo )
getElementsInPipeline (Model { pipeline }) =
    pipeline


{-| Resets the requests pipeline
-}
resetPipeline : Model name decodesTo msg -> Model name decodesTo msg
resetPipeline (Model modelData) =
    Model { modelData | pipeline = [] }


{-| Replace the existing pipeline with a new one
-}
setElementsInPipeline : List ( QueryId, PipelineElement name decodesTo ) -> Model name decodesTo msg -> Model name decodesTo msg
setElementsInPipeline pipeline (Model modelData) =
    Model { modelData | pipeline = pipeline }


{-| Removes a query from the pipeline
-}
removeQueryFromPipeline : QueryId -> Model name decodesTo msg -> Model name decodesTo msg
removeQueryFromPipeline queryId ((Model { pipeline }) as model) =
    pipeline
        |> List.filter ((/=) queryId << Tuple.first)
        |> flip setElementsInPipeline model


{-| Adds an element to the current pipeline
-}
addElementToPipeline : ( QueryId, PipelineElement name decodesTo ) -> Model name decodesTo msg -> Model name decodesTo msg
addElementToPipeline query ((Model modelData) as model) =
    Model { modelData | pipeline = query :: getElementsInPipeline model }


{-| Gets the message lifter
-}
getMsgLifter : Model name decodesTo msg -> (Msg name decodesTo -> msg)
getMsgLifter (Model { msgLifter }) =
    msgLifter


{-| Gets the error identifier
-}
getAuthenticationErrorIdentifier : Model name decodesTo msg -> (List GraphqlError -> Bool)
getAuthenticationErrorIdentifier (Model { authenticationErrorIdentifier }) =
    authenticationErrorIdentifier


{-| Gets the name of a pipeline element
-}
getPipelineElementName : PipelineElement name decodesTo -> name
getPipelineElementName pipelineElement =
    case pipelineElement of
        Query name _ ->
            name

        Mutation name _ ->
            name
