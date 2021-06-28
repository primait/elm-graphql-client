module GraphqlClient.Client exposing
    ( authenticationChanged
    , manageUpdate
    , runMutation
    , runQuery
    )

{-| This is a Graphql client with automatic retry

@docs authenticationChanged
@docs manageUpdate
@docs runMutation
@docs runQuery

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


{-| funzione che permette di eseguire una query graphql
ritorna una tupla con il modello aggiornato del client (da inserire nel modello del chiamante) e il comando da eseguire.
Il comando va ovviamente passato al runtime elm

I parametri sono:

  - il nome della query
  - una lista di header di autenticazione (name: String, value: String)
  - il modello del componente
  - il selection set della query da eseguire

-}
runQuery : name -> List Authentication -> Model name decodesTo msg -> SelectionSet decodesTo RootQuery -> ( Model name decodesTo msg, Cmd (Msg name decodesTo) )
runQuery name auth model =
    doRunQuery Transient auth model << M.Query name


{-| funzione che permette di eseguire una mutation graphql
ritorna una tupla con il modello aggiornato del client (da inserire nel modello del chiamante) e il comando da eseguire.
Il comando va ovviamente passato al runtime elm

I parametri sono:

  - il nome della mutation
  - una lista di header di autenticazione (name: String, value: String)
  - il modello del componente
  - il selection set della query da eseguire

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


{-| funzione da chiamare quando il chiamante è riuscito a recuperare un nuovo token
Questa funzione triggera l'esecuzione di tutte le query che erano fallite fino a questo punto ed erano rimaste in attesa di nuova autenticazione
I parametri sono:

  - il modello del componente
  - una lista di Authentication, tuple di (header name, header value) espressi tutti e due come String
  - una closure per aggiornare il modello del chiamante con il modello del componente. tipo (\\gqlClientModel -> {model | gqlClientModel = gqlClientModel})
  - il modello del chiamante

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


{-| HOF che permette il wiring di un componente GqlClient con il chiamante.
Va messo nell'Update del chiamante dove si ricevono i messaggi del componente
La sua funzione principale è quella di evitare che il chiamante debba ricordare tutti i dettagli implementativi di integrazione, con il rischio di dimenticarsi dei pezzi

I parametri sono:

  - il messaggio del componente
  - il modello del componente
  - il Cmd Msg del chiamante per ottenere un nuovo token
  - una closure per gestire il risultato di una query andata a buon fine. La closure accetta il modello del chiamante e il tipo di risposta della query eseguita (decodesTo)
  - una closure per gestire il risultato di una query andata in errore. La closure accetta il modello del chiamante e un Graphql.Http.Error
  - una closure per aggiornare il modello del chiamante con il modello del componente. tipo (\\gqlClientModel -> {model | gqlClientModel = gqlClientModel})
  - il modello del chiamante

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
