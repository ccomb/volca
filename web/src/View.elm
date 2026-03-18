module View exposing (View, defaultView, map)

import Html exposing (Html)


type alias View msg =
    { title : String
    , body : Html msg
    }


map : (msg1 -> msg) -> View msg1 -> View msg
map toMsg view =
    { title = view.title
    , body = Html.map toMsg view.body
    }


defaultView : View msg
defaultView =
    { title = "VoLCA"
    , body = Html.text "Loading..."
    }
