module GraphqlClient.Config exposing
    ( Config
    , getUrl
    , initialize
    )

{-| Config module
-}


{-| The Config type for the client
-}
type Config
    = Config
        { url : String
        }


{-| Config constructor
-}
initialize : String -> Config
initialize url =
    Config { url = url }


{-| Retrieve the url
-}
getUrl : Config -> String
getUrl (Config { url }) =
    url
