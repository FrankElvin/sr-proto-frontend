module Reports exposing (..)

import Browser
import Html exposing (Html, div, text, input)
import Html.Attributes exposing (placeholder, value )
import Html.Events exposing (onInput)


-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL
type alias CompanyReport =
  { id: Int
  , name: String
  , balance: Float
  }

type alias Model =
  { dbObtained: List CompanyReport
  , searchFilter: String
  , searchResult: List CompanyReport
  }

init : Model
init = 
  { dbObtained =
    [ CompanyReport 1 "Petya Company" 0.5
    , CompanyReport 2 "Sanya LTD" 1.5
    , CompanyReport 3 "Malika Industries" 0.7
    , CompanyReport 4 "Kate GMBH" 0.5
    , CompanyReport 5 "Kate's Nikita Technologies" 0.3
    ],
  searchFilter = "",
  searchResult = 
    [ CompanyReport 1 "Petya Company" 0.5
    , CompanyReport 2 "Sanya LTD" 1.5
    , CompanyReport 3 "Malika Industries" 0.7
    , CompanyReport 4 "Kate GMBH" 0.5
    , CompanyReport 5 "Kate's Nikita Technologies" 0.3
    ]
  }


-- UPDATE
type Msg = Search String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Search filter ->
      if (String.isEmpty filter) then 
        { model | searchFilter = filter, searchResult = model.dbObtained }
      else 
        { model |
          searchFilter = filter
        , searchResult = List.filter (isMatchingFilter filter) model.dbObtained
        }

isMatchingFilter : String -> CompanyReport -> Bool
isMatchingFilter filter report =
  if String.startsWith filter report.name then
    True
  else
    False


-- VIEW
view : Model -> Html Msg
view model = div [] 
  [ div [] [ text "Table of reports" ]
  , input [ placeholder "Filter by name", value model.searchFilter, onInput Search] []
  , div [] ( List.map reportView model.searchResult )
  ]

reportView : CompanyReport -> Html Msg
reportView report = div [] 
  [ text (String.fromInt report.id)
  , text " | "
  , text report.name
  , text " | "
  , text (String.fromFloat report.balance)
  ]
  
