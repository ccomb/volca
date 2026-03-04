module Route exposing
    ( Route(..)
    , ActivityTab(..)
    , ActivitiesFlags
    , DatabaseDetailFlags
    , FlowMappingFlags
    , LCIAFlags
    , MethodDetailFlags
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
    , matchDatabaseDetail
    , matchUpload
    , matchDatabaseSetup
    , matchMethods
    , matchMethodUpload
    , matchMethodDetail
    , matchFlowMapping
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
    | LCIARoute String String (Maybe String) -- db, processId, method collection
    | DatabasesRoute
    | DatabaseDetailRoute String (Maybe String) -- dbName, ?method=collection
    | UploadRoute
    | DatabaseSetupRoute String
    | MethodsRoute
    | MethodUploadRoute
    | MethodDetailRoute String (Maybe String) -- collection, ?db=dbName
    | FlowMappingRoute String (Maybe String) -- methodId, ?db=dbName
    | NotFoundRoute


{-| Simplified page identity for left menu highlighting.
-}
type ActivePage
    = HomeActive
    | ActivitiesActive
    | ActivityActive ActivityTab
    | DatabasesActive
    | DatabaseDetailActive
    | UploadActive
    | DatabaseSetupActive
    | MethodsActive
    | MethodUploadActive
    | MethodDetailActive
    | FlowMappingActive


routeToActivePage : Route -> ActivePage
routeToActivePage route =
    case route of
        ActivityRoute tab _ _ ->
            ActivityActive tab

        LCIARoute _ _ _ ->
            ActivityActive LCIA

        RootRoute ->
            HomeActive

        ActivitiesRoute _ ->
            ActivitiesActive

        DatabasesRoute ->
            DatabasesActive

        DatabaseDetailRoute _ _ ->
            DatabaseDetailActive

        UploadRoute ->
            UploadActive

        DatabaseSetupRoute _ ->
            DatabaseSetupActive

        MethodsRoute ->
            MethodsActive

        MethodUploadRoute ->
            MethodUploadActive

        MethodDetailRoute _ _ ->
            MethodDetailActive

        FlowMappingRoute _ _ ->
            FlowMappingActive

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

        LCIARoute _ _ method ->
            LCIARoute db pid method

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
        , Parser.map DatabaseDetailRoute (Parser.s "databases" </> string <?> Query.string "method")
        , Parser.map DatabasesRoute (Parser.s "databases")
        , Parser.map FlowMappingRoute (Parser.s "mapping" </> string <?> Query.string "db")
        , Parser.map MethodUploadRoute (Parser.s "methods" </> Parser.s "upload")
        , Parser.map MethodDetailRoute (Parser.s "methods" </> string <?> Query.string "db")
        , Parser.map MethodsRoute (Parser.s "methods")
        , Parser.map (\db query -> ActivitiesRoute { db = db, name = query.name, limit = query.limit })
            (Parser.s "db" </> string </> Parser.s "activities" <?> activitiesQueryParser)
        , Parser.map (ActivityRoute Upstream) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "upstream")
        , Parser.map (ActivityRoute Emissions) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "emissions")
        , Parser.map (ActivityRoute Resources) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "resources")
        , Parser.map (ActivityRoute Products) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "products")
        , Parser.map (ActivityRoute Tree) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "tree")
        , Parser.map (ActivityRoute Inventory) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "inventory")
        , Parser.map (ActivityRoute Graph) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "graph")
        , Parser.map LCIARoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "lcia" <?> Query.string "method")
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


appendQuery : List (Maybe Url.Builder.QueryParameter) -> String
appendQuery params =
    let
        filtered =
            List.filterMap identity params
    in
    if List.isEmpty filtered then
        ""

    else
        Url.Builder.toQuery filtered


routeToUrl : Route -> String
routeToUrl route =
    case route of
        RootRoute ->
            "/"

        ActivitiesRoute { db, name, limit } ->
            "/db/" ++ db ++ "/activities"
                ++ appendQuery
                    [ Maybe.map (Url.Builder.string "name") name
                    , Maybe.map (Url.Builder.int "limit") limit
                    ]

        ActivityRoute tab db processId ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/" ++ activityTabSlug tab

        LCIARoute db processId method ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/lcia"
                ++ appendQuery [ Maybe.map (Url.Builder.string "method") method ]

        DatabasesRoute ->
            "/databases"

        DatabaseDetailRoute dbName method ->
            "/databases/" ++ dbName
                ++ appendQuery [ Maybe.map (Url.Builder.string "method") method ]

        UploadRoute ->
            "/databases/upload"

        DatabaseSetupRoute dbName ->
            "/databases/" ++ dbName ++ "/setup"

        MethodsRoute ->
            "/methods"

        MethodUploadRoute ->
            "/methods/upload"

        MethodDetailRoute name db ->
            "/methods/" ++ name
                ++ appendQuery [ Maybe.map (Url.Builder.string "db") db ]

        FlowMappingRoute methodId db ->
            "/mapping/" ++ methodId
                ++ appendQuery [ Maybe.map (Url.Builder.string "db") db ]

        NotFoundRoute ->
            "/"


routeToDatabase : Route -> Maybe String
routeToDatabase route =
    case route of
        ActivitiesRoute { db } ->
            Just db

        ActivityRoute _ db _ ->
            Just db

        LCIARoute db _ _ ->
            Just db

        DatabaseSetupRoute dbName ->
            Just dbName

        DatabaseDetailRoute dbName _ ->
            Just dbName

        _ ->
            Nothing



-- Match Functions


type alias ActivitiesFlags =
    { db : String, name : Maybe String, limit : Maybe Int }


type alias LCIAFlags =
    { db : String, processId : String, method : Maybe String }


type alias MethodDetailFlags =
    { collection : String, db : Maybe String }


type alias DatabaseDetailFlags =
    { dbName : String, method : Maybe String }


type alias FlowMappingFlags =
    { methodId : String, db : Maybe String }


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


matchLCIA : Route -> Maybe LCIAFlags
matchLCIA route =
    case route of
        LCIARoute db pid method ->
            Just { db = db, processId = pid, method = method }

        _ ->
            Nothing


matchDatabases : Route -> Maybe ()
matchDatabases route =
    case route of
        DatabasesRoute ->
            Just ()

        _ ->
            Nothing


matchDatabaseDetail : Route -> Maybe DatabaseDetailFlags
matchDatabaseDetail route =
    case route of
        DatabaseDetailRoute dbName method ->
            Just { dbName = dbName, method = method }

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


matchMethods : Route -> Maybe ()
matchMethods route =
    case route of
        MethodsRoute ->
            Just ()

        _ ->
            Nothing


matchMethodUpload : Route -> Maybe ()
matchMethodUpload route =
    case route of
        MethodUploadRoute ->
            Just ()

        _ ->
            Nothing


matchMethodDetail : Route -> Maybe MethodDetailFlags
matchMethodDetail route =
    case route of
        MethodDetailRoute collection db ->
            Just { collection = collection, db = db }

        _ ->
            Nothing


matchFlowMapping : Route -> Maybe FlowMappingFlags
matchFlowMapping route =
    case route of
        FlowMappingRoute methodId db ->
            Just { methodId = methodId, db = db }

        _ ->
            Nothing
