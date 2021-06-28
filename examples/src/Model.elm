module Model exposing (..)

import Api
import Graphql.Http
import PrimaElm.Lib.GqlClient.Model as GqlClientModel


type alias Model =
    { result : Res
    , active : Bool
    , gqlClientModel : GqlClientModel.Model QueryName Api.QueryResult Msg
    , error : Maybe String
    , gqlClientAuthentication : List GqlClientModel.Authentication
    }


type QueryResult
    = Hello Api.HelloResult
    | HelloWithNum Api.HelloResultWithNum


gqlClientModelConfig : GqlClientModel.Model QueryName Api.QueryResult Msg
gqlClientModelConfig =
    GqlClientModel.initialize "http://localhost:8001" GqlClientMsg


type Res
    = NotAsked
    | Loading
    | Ok QueryResult


initialModel : Model
initialModel =
    { result = NotAsked
    , active = False
    , gqlClientModel = gqlClientModelConfig
    , error = Nothing
    , gqlClientAuthentication = []
    }


type QueryName
    = All


type Msg
    = DoQuery
    | DoQueryWithNum
    | DoMutationNewNumber
    | ReceivedToken Bool (Result (Graphql.Http.Error String) String)
    | GenerateNewToken
    | GqlClientMsg (GqlClientModel.Msg QueryName Api.QueryResult)


type alias Flags =
    {}


handleQueryResponse : Model -> QueryName -> Api.QueryResult -> ( Model, Cmd Msg )
handleQueryResponse model _ response =
    case response of
        Api.QueryResultReceivedData helloResult ->
            ( { model | result = Ok <| Hello helloResult }, Cmd.none )

        Api.QueryResultReceivedDataWithNum helloResultWithNum ->
            ( { model | result = Ok <| HelloWithNum helloResultWithNum }, Cmd.none )

        Api.QueryResultNewNumberMutation bool ->
            ( { model | result = NotAsked }, Cmd.none )


handleQueryError : Model -> QueryName -> Graphql.Http.Error Api.QueryResult -> Model
handleQueryError model _ error =
    let
        _ =
            Debug.log "error" error
    in
    { model | error = Just "errore" }


setGqlClientModel : GqlClientModel.Model QueryName Api.QueryResult Msg -> Model -> Model
setGqlClientModel gqlClientModel model =
    { model | gqlClientModel = gqlClientModel }
