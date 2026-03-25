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
    , matchFlowSynonyms
    , FlowSynonymDetailFlags
    , matchFlowSynonymDetail
    , DatabaseMappingFlags
    , matchDatabaseMapping
    , matchDatabaseMappingOverview
    , matchDatabaseDetailAsActivities
    , matchCompartmentMappings
    , matchUnits
    , CompositionFlags
    , matchComposition
    , matchConsumers
    , matchSupplyChainGraph
    , matchSupplyChainGraphDagre
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
    | LCIA
    | Consumers


type Route
    = RootRoute
    | ActivitiesRoute { db : String, name : Maybe String, product : Maybe String, limit : Maybe Int, classification : Maybe String, classificationValue : Maybe String }
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
    | CompositionRoute CompositionFlags
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
    | FlowSynonymsActive
    | CompositionActive
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

        FlowSynonymsRoute ->
            FlowSynonymsActive

        FlowSynonymDetailRoute _ ->
            FlowSynonymsActive

        DatabaseMappingRoute _ _ ->
            DatabaseMappingActive

        CompositionRoute _ ->
            CompositionActive

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

        CompositionRoute flags ->
            CompositionRoute { flags | db = db, processId = pid }

        _ ->
            ActivityRoute Upstream db pid



-- URL Parsing


activitiesQueryParser : Query.Parser { name : Maybe String, product : Maybe String, limit : Maybe Int, classification : Maybe String, classificationValue : Maybe String }
activitiesQueryParser =
    Query.map3
        (\base product cls ->
            { name = base.name
            , product = product
            , limit = base.limit
            , classification = cls.classification
            , classificationValue = cls.classificationValue
            }
        )
        (Query.map2 (\name limit -> { name = name, limit = limit })
            (Query.string "name")
            (Query.int "limit")
        )
        (Query.string "product")
        (Query.map2 (\cls clsVal -> { classification = cls, classificationValue = clsVal })
            (Query.string "classification")
            (Query.string "classification-value")
        )


compositionQueryParser : Query.Parser { name : Maybe String, location : Maybe String, classification : Maybe String, maxDepth : Maybe String, minQuantity : Maybe String, sort : Maybe String, order : Maybe String }
compositionQueryParser =
    Query.map2
        (\filters sorting ->
            { name = filters.name
            , location = filters.location
            , classification = filters.classification
            , maxDepth = filters.maxDepth
            , minQuantity = filters.minQuantity
            , sort = sorting.sort
            , order = sorting.order
            }
        )
        (Query.map2
            (\base extra -> { name = base.name, location = base.location, classification = extra.classification, maxDepth = extra.maxDepth, minQuantity = extra.minQuantity })
            (Query.map2 (\name location -> { name = name, location = location })
                (Query.string "name")
                (Query.string "location")
            )
            (Query.map2
                (\cls depths -> { classification = cls, maxDepth = depths.maxDepth, minQuantity = depths.minQuantity })
                (Query.string "classification")
                (Query.map2 (\md mq -> { maxDepth = md, minQuantity = mq })
                    (Query.string "max-depth")
                    (Query.string "min-quantity")
                )
            )
        )
        (Query.map2 (\s o -> { sort = s, order = o })
            (Query.string "sort")
            (Query.string "order")
        )


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
        , Parser.map (\db query -> ActivitiesRoute { db = db, name = query.name, product = query.product, limit = query.limit, classification = query.classification, classificationValue = query.classificationValue })
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
        , Parser.map LCIARoute (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "lcia" <?> Query.string "method")
        , Parser.map (\db pid query -> CompositionRoute { db = db, processId = pid, name = query.name, location = query.location, classification = query.classification, maxDepth = query.maxDepth, minQuantity = query.minQuantity, sort = query.sort, order = query.order })
            (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "composition" <?> compositionQueryParser)
        , Parser.map (ActivityRoute Consumers) (Parser.s "db" </> string </> Parser.s "activity" </> string </> Parser.s "consumers")
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

        LCIA ->
            "lcia"

        Consumers ->
            "consumers"


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

        ActivitiesRoute { db, name, product, limit, classification, classificationValue } ->
            "/db/" ++ db ++ "/activities"
                ++ appendQuery
                    [ Maybe.map (Url.Builder.string "name") name
                    , Maybe.map (Url.Builder.string "product") product
                    , Maybe.map (Url.Builder.int "limit") limit
                    , Maybe.map (Url.Builder.string "classification") classification
                    , Maybe.map (Url.Builder.string "classification-value") classificationValue
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

        FlowSynonymsRoute ->
            "/flow-synonyms"

        FlowSynonymDetailRoute name ->
            "/flow-synonyms/" ++ name

        CompositionRoute { db, processId, name, location, classification, maxDepth, minQuantity, sort, order } ->
            "/db/" ++ db ++ "/activity/" ++ processId ++ "/composition"
                ++ appendQuery
                    [ Maybe.map (Url.Builder.string "name") name
                    , Maybe.map (Url.Builder.string "location") location
                    , Maybe.map (Url.Builder.string "classification") classification
                    , Maybe.map (Url.Builder.string "max-depth") maxDepth
                    , Maybe.map (Url.Builder.string "min-quantity") minQuantity
                    , Maybe.map (Url.Builder.string "sort") sort
                    , Maybe.map (Url.Builder.string "order") order
                    ]

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

        CompositionRoute { db } ->
            Just db

        _ ->
            Nothing



-- Match Functions


type alias ActivitiesFlags =
    { db : String, name : Maybe String, product : Maybe String, limit : Maybe Int, classification : Maybe String, classificationValue : Maybe String }


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
            Just { db = dbName, name = Nothing, product = Nothing, limit = Just 20, classification = Nothing, classificationValue = Nothing }

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


type alias CompositionFlags =
    { db : String, processId : String, name : Maybe String, location : Maybe String, classification : Maybe String, maxDepth : Maybe String, minQuantity : Maybe String, sort : Maybe String, order : Maybe String }


matchComposition : Route -> Maybe CompositionFlags
matchComposition route =
    case route of
        CompositionRoute flags ->
            Just flags

        _ ->
            Nothing


matchConsumers : Route -> Maybe ( String, String )
matchConsumers =
    matchTab Consumers


matchSupplyChainGraph : Route -> Maybe ( String, String )
matchSupplyChainGraph =
    matchTab SupplyChainGraph


matchSupplyChainGraphDagre : Route -> Maybe ( String, String )
matchSupplyChainGraphDagre =
    matchTab SupplyChainGraphDagre
