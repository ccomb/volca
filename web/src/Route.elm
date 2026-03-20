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
    , matchSupplyChainGraph
    , matchSupplyChainGraphDagre
    , matchSupplyChainTable
    , matchVariant
    , FlowSearchFlags
    , matchFlowSearch
    , matchLCIA
    , matchDatabases
    , matchDatabaseDetail
    , matchUpload
    , matchDatabaseSetup
    , matchMethods
    , matchMethodUpload
    , matchMethodDetail
    , matchFlowMapping
    , matchFlowSynonyms
    , FlowSynonymDetailFlags
    , matchFlowSynonymDetail
    , DatabaseMappingFlags
    , matchDatabaseMapping
    , matchDatabaseMappingOverview
    , matchDatabaseDetailAsActivities
    , matchCompartmentMappings
    , matchUnits
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
    | SupplyChainGraph
    | SupplyChainGraphDagre
    | SupplyChainTable
    | Variant
    | LCIA


type Route
    = RootRoute
    | ActivitiesRoute { db : String, name : Maybe String, product : Maybe String, limit : Maybe Int }
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
    | FlowSynonymsRoute
    | FlowSynonymDetailRoute String -- resource name
    | DatabaseMappingRoute String (Maybe String) -- dbName, ?method=methodId
    | FlowSearchRoute { db : String, q : Maybe String, limit : Maybe Int, offset : Maybe Int }
    | CompartmentMappingsRoute
    | UnitsRoute
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
    | DatabaseMappingActive
    | FlowSearchActive
    | FlowSynonymsActive
    | CompartmentMappingsActive
    | UnitsActive


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

        FlowSearchRoute _ ->
            FlowSearchActive

        FlowSynonymsRoute ->
            FlowSynonymsActive

        FlowSynonymDetailRoute _ ->
            FlowSynonymsActive

        DatabaseMappingRoute _ _ ->
            DatabaseMappingActive

        CompartmentMappingsRoute ->
            CompartmentMappingsActive

        UnitsRoute ->
            UnitsActive

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


activitiesQueryParser : Query.Parser { name : Maybe String, product : Maybe String, limit : Maybe Int }
activitiesQueryParser =
    Query.map3 (\name product limit -> { name = name, product = product, limit = limit })
        (Query.string "name")
        (Query.string "product")
        (Query.int "limit")


flowSearchQueryParser : Query.Parser { q : Maybe String, limit : Maybe Int, offset : Maybe Int }
flowSearchQueryParser =
    Query.map3 (\q limit offset -> { q = q, limit = limit, offset = offset })
        (Query.string "q")
        (Query.int "limit")
        (Query.int "offset")


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Parser.map RootRoute top
        , Parser.map UploadRoute (Parser.s "databases" </> Parser.s "upload")
        , Parser.map DatabaseSetupRoute (Parser.s "databases" </> string </> Parser.s "setup")
        , Parser.map DatabaseMappingRoute (Parser.s "databases" </> string </> Parser.s "mapping" <?> Query.string "method")
        , Parser.map DatabaseDetailRoute (Parser.s "databases" </> string <?> Query.string "method")
        , Parser.map DatabasesRoute (Parser.s "databases")
        , Parser.map FlowMappingRoute (Parser.s "mapping" </> string <?> Query.string "db")
        , Parser.map MethodUploadRoute (Parser.s "methods" </> Parser.s "upload")
        , Parser.map MethodDetailRoute (Parser.s "methods" </> string <?> Query.string "db")
        , Parser.map MethodsRoute (Parser.s "methods")
        , Parser.map FlowSynonymDetailRoute (Parser.s "flow-synonyms" </> string)
        , Parser.map FlowSynonymsRoute (Parser.s "flow-synonyms")
        , Parser.map CompartmentMappingsRoute (Parser.s "compartment-mappings")
        , Parser.map UnitsRoute (Parser.s "units")
        , Parser.map (\db query -> ActivitiesRoute { db = db, name = query.name, product = query.product, limit = query.limit })
            (Parser.s "db" </> string </> Parser.s "activities" <?> activitiesQueryParser)
        , Parser.map (ActivityRoute Upstream) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "upstream")
        , Parser.map (ActivityRoute Emissions) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "emissions")
        , Parser.map (ActivityRoute Resources) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "resources")
        , Parser.map (ActivityRoute Products) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "products")
        , Parser.map (ActivityRoute Tree) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "tree")
        , Parser.map (ActivityRoute Inventory) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "inventory")
        , Parser.map (ActivityRoute Graph) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "graph")
        , Parser.map (ActivityRoute SupplyChainGraph) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "supply-chain-graph")
        , Parser.map (ActivityRoute SupplyChainGraphDagre) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "supply-chain-dagre")
        , Parser.map (ActivityRoute SupplyChainTable) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "supply-chain-table")
        , Parser.map (ActivityRoute Variant) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "variant")
        , Parser.map (\db query -> FlowSearchRoute { db = db, q = query.q, limit = query.limit, offset = query.offset })
            (Parser.s "db" </> string </> Parser.s "flows" <?> flowSearchQueryParser)
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

        SupplyChainGraph ->
            "supply-chain-graph"

        SupplyChainGraphDagre ->
            "supply-chain-dagre"

        SupplyChainTable ->
            "supply-chain-table"

        Variant ->
            "variant"

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

        ActivitiesRoute { db, name, product, limit } ->
            "/db/" ++ db ++ "/activities"
                ++ appendQuery
                    [ Maybe.map (Url.Builder.string "name") name
                    , Maybe.map (Url.Builder.string "product") product
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

        DatabaseMappingRoute dbName method ->
            "/databases/" ++ dbName ++ "/mapping"
                ++ appendQuery [ Maybe.map (Url.Builder.string "method") method ]

        FlowSearchRoute { db, q, limit, offset } ->
            "/db/" ++ db ++ "/flows"
                ++ appendQuery
                    [ Maybe.map (Url.Builder.string "q") q
                    , Maybe.map (Url.Builder.int "limit") limit
                    , Maybe.map (Url.Builder.int "offset") offset
                    ]

        FlowSynonymsRoute ->
            "/flow-synonyms"

        FlowSynonymDetailRoute name ->
            "/flow-synonyms/" ++ name

        CompartmentMappingsRoute ->
            "/compartment-mappings"

        UnitsRoute ->
            "/units"

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

        DatabaseMappingRoute dbName _ ->
            Just dbName

        FlowSearchRoute { db } ->
            Just db

        _ ->
            Nothing



-- Match Functions


type alias ActivitiesFlags =
    { db : String, name : Maybe String, product : Maybe String, limit : Maybe Int }


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


matchSupplyChainGraph : Route -> Maybe ( String, String )
matchSupplyChainGraph =
    matchTab SupplyChainGraph


matchSupplyChainGraphDagre : Route -> Maybe ( String, String )
matchSupplyChainGraphDagre =
    matchTab SupplyChainGraphDagre


matchSupplyChainTable : Route -> Maybe ( String, String )
matchSupplyChainTable =
    matchTab SupplyChainTable


matchVariant : Route -> Maybe ( String, String )
matchVariant =
    matchTab Variant


type alias FlowSearchFlags =
    { db : String, q : Maybe String, limit : Maybe Int, offset : Maybe Int }


matchFlowSearch : Route -> Maybe FlowSearchFlags
matchFlowSearch route =
    case route of
        FlowSearchRoute flags ->
            Just flags

        _ ->
            Nothing


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


matchFlowSynonyms : Route -> Maybe ()
matchFlowSynonyms route =
    case route of
        FlowSynonymsRoute ->
            Just ()

        _ ->
            Nothing


type alias FlowSynonymDetailFlags =
    { name : String }


matchFlowSynonymDetail : Route -> Maybe FlowSynonymDetailFlags
matchFlowSynonymDetail route =
    case route of
        FlowSynonymDetailRoute name ->
            Just { name = name }

        _ ->
            Nothing


type alias DatabaseMappingFlags =
    { dbName : String, method : Maybe String }


matchDatabaseMapping : Route -> Maybe DatabaseMappingFlags
matchDatabaseMapping route =
    case route of
        DatabaseMappingRoute dbName method ->
            Just { dbName = dbName, method = method }

        _ ->
            Nothing


{-| Match DatabaseMappingRoute — shows the mapping overview page.
The optional method query param persists the selected collection.
-}
matchDatabaseMappingOverview : Route -> Maybe DatabaseMappingFlags
matchDatabaseMappingOverview route =
    case route of
        DatabaseMappingRoute dbName method ->
            Just { dbName = dbName, method = method }

        _ ->
            Nothing


{-| Old /databases/{dbName} URL redirects to activities.
-}
matchDatabaseDetailAsActivities : Route -> Maybe ActivitiesFlags
matchDatabaseDetailAsActivities route =
    case route of
        DatabaseDetailRoute dbName _ ->
            Just { db = dbName, name = Nothing, product = Nothing, limit = Just 20 }

        _ ->
            Nothing


matchCompartmentMappings : Route -> Maybe ()
matchCompartmentMappings route =
    case route of
        CompartmentMappingsRoute ->
            Just ()

        _ ->
            Nothing


matchUnits : Route -> Maybe ()
matchUnits route =
    case route of
        UnitsRoute ->
            Just ()

        _ ->
            Nothing
