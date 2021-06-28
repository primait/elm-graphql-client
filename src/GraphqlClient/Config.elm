module GraphqlClient.Config exposing
    ( Config
    , getUrl
    , initialize
    )


type Config
    = Config
        { url : String
        }


initialize : String -> Config
initialize url =
    Config { url = url }


getUrl : Config -> String
getUrl (Config { url }) =
    url
