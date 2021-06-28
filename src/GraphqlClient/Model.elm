module GraphqlClient.Model exposing
    ( Authentication
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
    , removeQueryFromPipeline
    , resetPipelines
    , withAuthenticationErrorIdentifier
    )

import Dict
import Graphql.Http
import Graphql.Http.GraphqlError exposing (GraphqlError)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
import GraphqlClient.Config as Config exposing (Config)
import GraphqlClient.Utils exposing (find, flip, isJust)
import Json.Decode exposing (Decoder, at, bool, decodeValue)
import RemoteData exposing (RemoteData)


type alias HeaderName =
    String


type alias HeaderValue =
    String


type alias Authentication =
    ( HeaderName, HeaderValue )


type alias QueryId =
    Int


type PipelineElement name decodesTo
    = Query name (SelectionSet decodesTo RootQuery)
    | Mutation name (SelectionSet decodesTo RootMutation)


type Model name decodesTo msg
    = Model
        { config : Config
        , pipeline : List ( QueryId, PipelineElement name decodesTo )
        , msgLifter : Msg name decodesTo -> msg
        , authenticationErrorIdentifier : List GraphqlError -> Bool
        }


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


type Msg name decodesTo
    = FirstAttemptFinished QueryId name (RemoteData (Graphql.Http.Error decodesTo) decodesTo)
    | SecondAttemptFinished QueryId name (RemoteData (Graphql.Http.Error decodesTo) decodesTo)


getConfig : Model name decodesTo msg -> Config
getConfig (Model { config }) =
    config


getUrl : Model name decodesTo msg -> String
getUrl =
    Config.getUrl << getConfig


getElementsInPipeline : Model name decodesTo msg -> List ( QueryId, PipelineElement name decodesTo )
getElementsInPipeline (Model { pipeline }) =
    pipeline


resetPipelines : Model name decodesTo msg -> Model name decodesTo msg
resetPipelines (Model modelData) =
    Model { modelData | pipeline = [] }


setElementsInPipeline : List ( QueryId, PipelineElement name decodesTo ) -> Model name decodesTo msg -> Model name decodesTo msg
setElementsInPipeline pipeline (Model modelData) =
    Model { modelData | pipeline = pipeline }


removeQueryFromPipeline : QueryId -> Model name decodesTo msg -> Model name decodesTo msg
removeQueryFromPipeline queryId ((Model { pipeline }) as model) =
    pipeline
        |> List.filter ((/=) queryId << Tuple.first)
        |> flip setElementsInPipeline model


addElementToPipeline : ( QueryId, PipelineElement name decodesTo ) -> Model name decodesTo msg -> Model name decodesTo msg
addElementToPipeline query ((Model modelData) as model) =
    Model { modelData | pipeline = query :: getElementsInPipeline model }


getMsgLifter : Model name decodesTo msg -> (Msg name decodesTo -> msg)
getMsgLifter (Model { msgLifter }) =
    msgLifter


getAuthenticationErrorIdentifier : Model name decodesTo msg -> (List GraphqlError -> Bool)
getAuthenticationErrorIdentifier (Model { authenticationErrorIdentifier }) =
    authenticationErrorIdentifier


withAuthenticationErrorIdentifier : (List GraphqlError -> Bool) -> Model name decodesTo msg -> Model name decodesTo msg
withAuthenticationErrorIdentifier authenticationErrorIdentifier (Model modelData) =
    Model { modelData | authenticationErrorIdentifier = authenticationErrorIdentifier }


getPipelineElementName : PipelineElement name decodesTo -> name
getPipelineElementName pipelineElement =
    case pipelineElement of
        Query name _ ->
            name

        Mutation name _ ->
            name
