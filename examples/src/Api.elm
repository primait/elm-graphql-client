module Api exposing (..)

import Api.Mutation as Mutation
import Api.Object exposing (Hello)
import Api.Object.Hello as HelloObject
import Api.Query as Query
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SS exposing (SelectionSet)


type QueryResult
    = QueryResultReceivedData HelloResult
    | QueryResultReceivedDataWithNum HelloResultWithNum
    | QueryResultNewNumberMutation Bool


type alias HelloResult =
    { message : String
    }


helloQuery : SelectionSet QueryResult RootQuery
helloQuery =
    SS.map QueryResultReceivedData <| Query.hello helloQuerySelection


helloQuerySelection : SelectionSet HelloResult Hello
helloQuerySelection =
    SS.map HelloResult
        HelloObject.message


type alias HelloResultWithNum =
    { message : String
    , num : Int
    }


helloQueryNum : SelectionSet QueryResult RootQuery
helloQueryNum =
    SS.map QueryResultReceivedDataWithNum <| Query.hello helloQuerySelectionWithNum


helloQuerySelectionWithNum : SelectionSet HelloResultWithNum Hello
helloQuerySelectionWithNum =
    SS.map2 HelloResultWithNum
        HelloObject.message
        HelloObject.num


newNumberMutation : SelectionSet QueryResult RootMutation
newNumberMutation =
    Mutation.newNumber
        |> SS.map (always True)
        |> SS.map QueryResultNewNumberMutation
