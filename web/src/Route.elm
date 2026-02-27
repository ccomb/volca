module Route exposing
    ( Route(..)
    , ActivityTab(..)
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
    , withActivity
    )

import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, string, top)
import Url.Parser.Query as Query


type ActivityTab
    = Upstream
    | Emissions
    | Resources
    | Products
    | Tree
    | Inventory
    | Graph
    | LCIA


type Route
    = RootRoute
    | ActivitiesRoute { db : String, name : Maybe String, limit : Maybe Int }
    | ActivityRoute ActivityTab String String -- tab, db, processId
    | DatabasesRoute
    | UploadRoute
    | DatabaseSetupRoute String
    | NotFoundRoute


{-| Simplified page identity for left menu highlighting.
-}
type ActivePage
    = HomeActive
    | ActivitiesActive
    | ActivityActive ActivityTab
    | DatabasesActive
    | UploadActive
    | DatabaseSetupActive


routeToActivePage : Route -> ActivePage
routeToActivePage route =
    case route of
        ActivityRoute tab _ _ ->
            ActivityActive tab

        RootRoute ->
            HomeActive

        ActivitiesRoute _ ->
            ActivitiesActive

        DatabasesRoute ->
            DatabasesActive

        UploadRoute ->
            UploadActive

        DatabaseSetupRoute _ ->
            DatabaseSetupActive

        NotFoundRoute ->
            ActivitiesActive


{-| Rewrite the db/processId inside an ActivityRoute, preserving the tab.
Non-activity routes fall back to ActivityRoute Upstream.
-}
withActivity : String -> String -> Route -> Route
withActivity db pid route =
    case route of
        ActivityRoute tab _ _ ->
            ActivityRoute tab db pid

        _ ->
            ActivityRoute Upstream db pid



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
        , Parser.map (ActivityRoute Upstream) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "upstream")
        , Parser.map (ActivityRoute Emissions) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "emissions")
        , Parser.map (ActivityRoute Resources) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "resources")
        , Parser.map (ActivityRoute Products) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "products")
        , Parser.map (ActivityRoute Tree) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "tree")
        , Parser.map (ActivityRoute Inventory) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "inventory")
        , Parser.map (ActivityRoute Graph) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "graph")
        , Parser.map (ActivityRoute LCIA) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "lcia")
        , Parser.map (ActivityRoute Upstream) (Parser.s "db" </> string </> Parser.s "activity" </> string)
        ]


toRoute : Url -> Route
toRoute url =
    Parser.parse routeParser url
        |> Maybe.withDefault NotFoundRoute


activityTabSlug : ActivityTab -> String
activityTabSlug tab =
    case tab of
        Upstream ->
            "upstream"

        Emissions ->
            "emissions"

        Resources ->
            "resources"

        Products ->
            "products"

        Tree ->
            "tree"

        Inventory ->
            "inventory"

        Graph ->
            "graph"

        LCIA ->
            "lcia"


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

        ActivityRoute tab db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/" ++ activityTabSlug tab

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
        ActivitiesRoute { db } ->
            Just db

        ActivityRoute _ db _ ->
            Just db

        DatabaseSetupRoute dbName ->
            Just dbName

        _ ->
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


matchTab : ActivityTab -> Route -> Maybe ( String, String )
matchTab target route =
    case route of
        ActivityRoute tab db pid ->
            if tab == target then
                Just ( db, pid )

            else
                Nothing

        _ ->
            Nothing


matchUpstream : Route -> Maybe ( String, String )
matchUpstream route =
    case route of
        ActivityRoute Upstream db pid ->
            Just ( db, pid )

        _ ->
            Nothing


matchEmissions : Route -> Maybe ( String, String )
matchEmissions =
    matchTab Emissions


matchResources : Route -> Maybe ( String, String )
matchResources =
    matchTab Resources


matchProducts : Route -> Maybe ( String, String )
matchProducts =
    matchTab Products


matchTree : Route -> Maybe ( String, String )
matchTree =
    matchTab Tree


matchInventory : Route -> Maybe ( String, String )
matchInventory =
    matchTab Inventory


matchGraph : Route -> Maybe ( String, String )
matchGraph =
    matchTab Graph


matchLCIA : Route -> Maybe ( String, String )
matchLCIA =
    matchTab LCIA


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
