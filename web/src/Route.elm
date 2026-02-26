module Route exposing
    ( Route(..)
    , ActivitiesFlags
    , toRoute
    , routeToUrl
    , matchActivities
    , matchUpstream
    , matchEmissions
    , matchResources
    , matchProducts
    , matchTree
    , matchInventory
    , matchGraph
    , matchLCIA
    , matchDatabases
    , matchUpload
    , matchDatabaseSetup
    , matchHome
    , routeToDatabase
    , ActivePage(..)
    , routeToActivePage
    )

import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, string, top)
import Url.Parser.Query as Query


type Route
    = RootRoute
    | ActivitiesRoute { db : String, name : Maybe String, limit : Maybe Int }
    | ActivityRoute String String
    | ActivityUpstreamRoute String String
    | ActivityEmissionsRoute String String
    | ActivityResourcesRoute String String
    | ActivityProductsRoute String String
    | ActivityTreeRoute String String
    | ActivityInventoryRoute String String
    | ActivityGraphRoute String String
    | ActivityLCIARoute String String
    | DatabasesRoute
    | UploadRoute
    | DatabaseSetupRoute String
    | NotFoundRoute


{-| Simplified page identity for left menu highlighting.
Replaces the old Page type.
-}
type ActivePage
    = HomeActive
    | ActivitiesActive
    | UpstreamActive
    | EmissionsActive
    | ResourcesActive
    | ProductsActive
    | TreeActive
    | InventoryActive
    | GraphActive
    | LCIAActive
    | DatabasesActive
    | UploadActive
    | DatabaseSetupActive


routeToActivePage : Route -> ActivePage
routeToActivePage route =
    case route of
        RootRoute ->
            HomeActive

        ActivitiesRoute _ ->
            ActivitiesActive

        ActivityRoute _ _ ->
            UpstreamActive

        ActivityUpstreamRoute _ _ ->
            UpstreamActive

        ActivityEmissionsRoute _ _ ->
            EmissionsActive

        ActivityResourcesRoute _ _ ->
            ResourcesActive

        ActivityProductsRoute _ _ ->
            ProductsActive

        ActivityTreeRoute _ _ ->
            TreeActive

        ActivityInventoryRoute _ _ ->
            InventoryActive

        ActivityGraphRoute _ _ ->
            GraphActive

        ActivityLCIARoute _ _ ->
            LCIAActive

        DatabasesRoute ->
            DatabasesActive

        UploadRoute ->
            UploadActive

        DatabaseSetupRoute _ ->
            DatabaseSetupActive

        NotFoundRoute ->
            ActivitiesActive



-- URL Parsing


activitiesQueryParser : Query.Parser { name : Maybe String, limit : Maybe Int }
activitiesQueryParser =
    Query.map2 (\name limit -> { name = name, limit = limit })
        (Query.string "name")
        (Query.int "limit")


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map RootRoute top
        , Parser.map UploadRoute (Parser.s "databases" </> Parser.s "upload")
        , Parser.map DatabaseSetupRoute (Parser.s "databases" </> string </> Parser.s "setup")
        , Parser.map DatabasesRoute (Parser.s "databases")
        , Parser.map (\db query -> ActivitiesRoute { db = db, name = query.name, limit = query.limit })
            (Parser.s "db" </> string </> Parser.s "activities" <?> activitiesQueryParser)
        , Parser.map ActivityUpstreamRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "upstream")
        , Parser.map ActivityEmissionsRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "emissions")
        , Parser.map ActivityResourcesRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "resources")
        , Parser.map ActivityProductsRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "products")
        , Parser.map ActivityTreeRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "tree")
        , Parser.map ActivityInventoryRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "inventory")
        , Parser.map ActivityGraphRoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "graph")
        , Parser.map ActivityLCIARoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "lcia")
        , Parser.map ActivityRoute (Parser.s "db" </> string </> Parser.s "activity" </> string)
        ]


toRoute : Url -> Route
toRoute url =
    Parser.parse routeParser url
        |> Maybe.withDefault NotFoundRoute


routeToUrl : Route -> String
routeToUrl route =
    case route of
        RootRoute ->
            "/"

        ActivitiesRoute { db, name, limit } ->
            let
                queryParams =
                    [ Maybe.map (Url.Builder.string "name") name
                    , Maybe.map (Url.Builder.int "limit") limit
                    ]
                        |> List.filterMap identity
            in
            "/db/" ++ db ++ "/activities"
                ++ (if List.isEmpty queryParams then
                        ""

                    else
                        Url.Builder.toQuery queryParams
                   )

        ActivityRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId

        ActivityUpstreamRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/upstream"

        ActivityEmissionsRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/emissions"

        ActivityResourcesRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/resources"

        ActivityProductsRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/products"

        ActivityTreeRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/tree"

        ActivityInventoryRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/inventory"

        ActivityGraphRoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/graph"

        ActivityLCIARoute db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/lcia"

        DatabasesRoute ->
            "/databases"

        UploadRoute ->
            "/databases/upload"

        DatabaseSetupRoute dbName ->
            "/databases/" ++ dbName ++ "/setup"

        NotFoundRoute ->
            "/"


routeToDatabase : Route -> Maybe String
routeToDatabase route =
    case route of
        RootRoute ->
            Nothing

        ActivitiesRoute { db } ->
            Just db

        ActivityRoute db _ ->
            Just db

        ActivityUpstreamRoute db _ ->
            Just db

        ActivityEmissionsRoute db _ ->
            Just db

        ActivityResourcesRoute db _ ->
            Just db

        ActivityProductsRoute db _ ->
            Just db

        ActivityTreeRoute db _ ->
            Just db

        ActivityInventoryRoute db _ ->
            Just db

        ActivityGraphRoute db _ ->
            Just db

        ActivityLCIARoute db _ ->
            Just db

        DatabasesRoute ->
            Nothing

        UploadRoute ->
            Nothing

        DatabaseSetupRoute dbName ->
            Just dbName

        NotFoundRoute ->
            Nothing



-- Match Functions


type alias ActivitiesFlags =
    { db : String, name : Maybe String, limit : Maybe Int }


matchHome : Route -> Maybe ()
matchHome route =
    case route of
        RootRoute ->
            Just ()

        _ ->
            Nothing


matchActivities : Route -> Maybe ActivitiesFlags
matchActivities route =
    case route of
        ActivitiesRoute flags ->
            Just flags

        _ ->
            Nothing


matchUpstream : Route -> Maybe ( String, String )
matchUpstream route =
    case route of
        ActivityRoute db pid ->
            Just ( db, pid )

        ActivityUpstreamRoute db pid ->
            Just ( db, pid )

        _ ->
            Nothing


matchEmissions : Route -> Maybe ( String, String )
matchEmissions route =
    case route of
        ActivityEmissionsRoute db pid ->
            Just ( db, pid )

        _ ->
            Nothing


matchResources : Route -> Maybe ( String, String )
matchResources route =
    case route of
        ActivityResourcesRoute db pid ->
            Just ( db, pid )

        _ ->
            Nothing


matchProducts : Route -> Maybe ( String, String )
matchProducts route =
    case route of
        ActivityProductsRoute db pid ->
            Just ( db, pid )

        _ ->
            Nothing


matchTree : Route -> Maybe ( String, String )
matchTree route =
    case route of
        ActivityTreeRoute db pid ->
            Just ( db, pid )

        _ ->
            Nothing


matchInventory : Route -> Maybe ( String, String )
matchInventory route =
    case route of
        ActivityInventoryRoute db pid ->
            Just ( db, pid )

        _ ->
            Nothing


matchGraph : Route -> Maybe ( String, String )
matchGraph route =
    case route of
        ActivityGraphRoute db pid ->
            Just ( db, pid )

        _ ->
            Nothing


matchLCIA : Route -> Maybe ( String, String )
matchLCIA route =
    case route of
        ActivityLCIARoute db pid ->
            Just ( db, pid )

        _ ->
            Nothing


matchDatabases : Route -> Maybe ()
matchDatabases route =
    case route of
        DatabasesRoute ->
            Just ()

        _ ->
            Nothing


matchUpload : Route -> Maybe ()
matchUpload route =
    case route of
        UploadRoute ->
            Just ()

        _ ->
            Nothing


matchDatabaseSetup : Route -> Maybe String
matchDatabaseSetup route =
    case route of
        DatabaseSetupRoute name ->
            Just name

        _ ->
            Nothing
