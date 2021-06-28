module Auth exposing (..)

import Auth.Mutation as Mutation
import Auth.Query as Query
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet as SS exposing (SelectionSet)


fetchTokenQuery : SelectionSet String RootQuery
fetchTokenQuery =
    Query.token


generateNewToken : SelectionSet String RootMutation
generateNewToken =
    Mutation.newToken
