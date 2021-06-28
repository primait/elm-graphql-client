module GraphqlClient.Client exposing
    ( runQuery
    , runMutation
    , authenticationChanged
    , manageUpdate
    )

{-| This is a Graphql client with automatic retry

@docs runQuery
@docs runMutation
@docs authenticationChanged
@docs manageUpdate

-}

import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
import GraphqlClient.Model as M exposing (Authentication, Model, Msg(..))
import GraphqlClient.Response as Response
import GraphqlClient.Update as Update
import GraphqlClient.Utils exposing (flip)
import RemoteData as RD


type Attempt
    = Transient
    | Definitive


{-| Function that execute a Graphql query
It return a tuple with the updated internal model (to be inserted in the caller model) and the Cmd to be executed.

The arguments are:

  - the query name
  - a list of headers (name: String, value: String)
  - the component model
  - the selection set of the query that will be executed

-}
runQuery : name -> List Authentication -> Model name decodesTo msg -> SelectionSet decodesTo RootQuery -> ( Model name decodesTo msg, Cmd (Msg name decodesTo) )
runQuery name auth model =
    doRunQuery Transient auth model << M.Query name


{-| Function that execute a Graphql mutation
It return a tuple with the updated internal model (to be inserted in the caller model) and the Cmd to be executed.

The arguments are:

  - the mutation name
  - a list of headers (name: String, value: String)
  - the component model
  - the selection set of the query that will be executed

-}
runMutation : name -> List Authentication -> Model name decodesTo msg -> SelectionSet decodesTo RootMutation -> ( Model name decodesTo msg, Cmd (Msg name decodesTo) )
runMutation name auth model =
    doRunQuery Transient auth model << M.Mutation name


doRunQuery : Attempt -> List Authentication -> Model name decodesTo msg -> M.PipelineElement name decodesTo -> ( Model name decodesTo msg, Cmd (Msg name decodesTo) )
doRunQuery attempt auth model pipelineElement =
    let
        queryId : M.QueryId
        queryId =
            model
                |> M.getElementsInPipeline
                |> List.length
                |> (+) 1

        request : Graphql.Http.Request decodesTo
        request =
            case pipelineElement of
                M.Query _ selectionSet ->
                    Graphql.Http.queryRequest (M.getUrl model) selectionSet

                M.Mutation _ selectionSet ->
                    Graphql.Http.mutationRequest (M.getUrl model) selectionSet

        authenticatedQuery : Graphql.Http.Request decodesTo
        authenticatedQuery =
            auth
                |> List.foldl
                    (\authHeader ->
                        Graphql.Http.withHeader (Tuple.first authHeader) (Tuple.second authHeader)
                    )
                    request

        tagger : RD.RemoteData (Graphql.Http.Error decodesTo) decodesTo -> Msg name decodesTo
        tagger =
            case attempt of
                Transient ->
                    FirstAttemptFinished queryId (M.getPipelineElementName pipelineElement)

                Definitive ->
                    SecondAttemptFinished queryId (M.getPipelineElementName pipelineElement)
    in
    ( M.addElementToPipeline ( queryId, pipelineElement ) model
    , Graphql.Http.send (RD.fromResult >> tagger) authenticatedQuery
    )


{-| Function to call when the caller has finished its "job" and it's ready to perform the query/mutation again.
If there were queries/mutations in pipeline, that were blocked by an authentication problem, they are all returned as a Cmd to be executed.

The arguments are:

  - the component model
  - a list of headers (name: String, value: String)
  - a closure to updater the caller model with the component model. Something like (\\gqlClientModel -> {model | gqlClientModel = gqlClientModel})
  - the caller model

-}
authenticationChanged :
    Model name decodesTo msg
    -> List Authentication
    -> (Model name decodesTo msg -> model -> model)
    -> model
    -> ( model, Cmd msg )
authenticationChanged model auth mainModelUpdater mainModel =
    let
        queries : List ( M.QueryId, M.PipelineElement name decodesTo )
        queries =
            M.getElementsInPipeline model

        queryFold : ( M.QueryId, M.PipelineElement name decodesTo ) -> ( Model name decodesTo msg, List (Cmd (Msg name decodesTo)) ) -> ( Model name decodesTo msg, List (Cmd (Msg name decodesTo)) )
        queryFold ( _, query ) ( accModel, commands ) =
            doRunQuery Definitive auth accModel query
                |> Tuple.mapSecond (flip (::) commands)
    in
    queries
        |> List.foldl queryFold ( model, [] )
        |> Tuple.mapFirst (flip mainModelUpdater mainModel << M.resetPipelines)
        |> Tuple.mapSecond (Cmd.map (M.getMsgLifter model) << Cmd.batch)


{-| This is a function that simplifies the wiring between the caller and the graphql client component.
It should be put inside the caller Update cycle.
The main purpose is to make it easier for the caller to remember how the integration should be done, by passing arguments to this function.
It is not mandatory for this reason, but highly recommended.

The arguments are:

  - the component message
  - the component model
  - the caller Cmd Msg to get a new header
  - a closure to handle the result of an OK query. The closure should accept the caller model and a decoder for the response
  - a closure to handle the result of an Error query. The closure should accept the caller model and a Graphql.Http.Error
  - a closure to updater the caller model with the component model. Something like (\\gqlClientModel -> {model | gqlClientModel = gqlClientModel})
  - the caller model

-}
manageUpdate :
    Msg name decodesTo
    -> Model name decodesTo msg
    -> Cmd msg
    -> (model -> name -> decodesTo -> ( model, Cmd msg ))
    -> (model -> name -> Graphql.Http.Error decodesTo -> model)
    -> (Model name decodesTo msg -> model -> model)
    -> model
    -> ( model, Cmd msg )
manageUpdate msg model tokenUpdater queryResultHandler queryErrorHandler mainModelUpdater mainModel =
    let
        ( gqlClientModel, gqlClientCmd, response ) =
            Update.update msg model

        ( newMainModel, cmd ) =
            case response of
                Response.NotAsked ->
                    ( mainModel, Cmd.none )

                Response.ResponseOk name remoteData ->
                    remoteData
                        |> RD.map (queryResultHandler mainModel name)
                        |> RD.withDefault ( mainModel, Cmd.none )

                Response.RefreshTokenExpired ->
                    ( mainModel, Cmd.none )

                Response.DefinitiveError name error ->
                    ( queryErrorHandler mainModel name error, Cmd.none )

        command : Cmd msg
        command =
            case response of
                Response.NotAsked ->
                    Cmd.none

                Response.ResponseOk _ _ ->
                    Cmd.none

                Response.RefreshTokenExpired ->
                    tokenUpdater

                Response.DefinitiveError _ _ ->
                    Cmd.none
    in
    ( mainModelUpdater gqlClientModel newMainModel, Cmd.batch [ command, Cmd.map (M.getMsgLifter model) gqlClientCmd, cmd ] )
