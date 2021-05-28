module AllReports exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Round

import Graphql.Http
import RemoteData exposing (RemoteData)
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import ReportApi.Object
import ReportApi.Object.CompanyReport as CompanyReport
import ReportApi.Query as Query
import ReportApi.Scalar as Scalar


-- MAIN
main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL
type alias Model =
  { data: ResponseModel
  , filteredData: List (Maybe CompanyReport)
  , searchFilter: String
  , statusText: Maybe String
  }
 
init : () -> (Model, Cmd Msg)
init _ =
  (Model RemoteData.NotAsked [] "" Nothing, makeRequest)


-- UPDATE
type Msg 
  = FilterData String
  | StartSearch 
  | LoadReport ResponseModel

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartSearch ->
      ( model, makeRequest )

    FilterData filter ->
      ( { model | searchFilter = filter, filteredData = filterModelData model.data filter model.filteredData }, Cmd.none)

    LoadReport responseModel -> 
      ( { model | data=responseModel, filteredData = filterModelData responseModel model.searchFilter model.filteredData  }, Cmd.none)

filterModelData : ResponseModel -> String -> List (Maybe CompanyReport) -> List (Maybe CompanyReport)
filterModelData responseModel filter currentData =
  case responseModel of
    RemoteData.NotAsked -> currentData
    RemoteData.Loading -> currentData
    RemoteData.Failure _ -> currentData
    RemoteData.Success reports -> List.filter (isMatchingFilter filter) (unpackRequestResult reports)

unpackRequestResult : Maybe (List (Maybe CompanyReport)) -> List (Maybe CompanyReport)
unpackRequestResult maybeReports = 
  case maybeReports of
    Nothing -> []
    Just reports -> reports


isMatchingFilter : String -> Maybe CompanyReport -> Bool
isMatchingFilter filter report =
  case report of
    Nothing -> False
    Just someReport ->
      case someReport.companyName of
        Nothing -> False
        Just name ->
          if String.startsWith filter name then
            True
          else
            False



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW
view : Model -> Document Msg
view model =
  Document "SR Proto Report Viewer" 
  -- [div []
  --   [ h2 [ class "title is-size-4"] [ text "Campaign report loader" ]
  --   , div [] [
  --       input [ placeholder "Report search filter", onInput FilterData ] []
  --     , button [ onClick StartSearch ] [ text "Update data" ]
  --     ]
  --   , statusTextBar model
  --   , reportTable model.filteredData
  --   ]
  -- ]
  [ nav [class "breadcrumb is-centered is-large"]
    [ ul []
      [ li [] [text "Home"]
      , li [class "is-active"] [text "Report Viewer"]
      ]
    ]
  , section [class "section"]
    [ div [class "columns"]
      [
        div [class "column is-10 is-offset-1"] 
        [ h1 [class "title"] [text "Campaign Report Viewer"]
        , h2 [class "subtitle"][text "Download company report data and filter it for analysis."]
        , div [class "columns"]
          [ div [class "column is-3"] [input [placeholder "Report search filter", onInput FilterData, class "input is-info"] []]
          , div [class "column is-1"] [button [onClick StartSearch, class "button is info"] [text "Update data"] ]
          ]
        , statusTextBar model
        ]
      ]
    ]
  , div [class "columns"]
    [ div [class "column is-10 is-offset-1"] 
      [ reportTable model.filteredData
      ]
    ]
   
  ]

type alias Document msg = 
  { title: String
  , body: List (Html msg)
  }


statusTextBar : Model -> Html Msg
statusTextBar model = 
  case model.data of
    RemoteData.NotAsked -> div [] []
    RemoteData.Loading -> div [] [text "Loading..."]
    RemoteData.Failure _ -> div [] [text "Failed to load reports"]
    RemoteData.Success _ -> div [] []

reportToRow : Maybe CompanyReport -> Html Msg
reportToRow maybeReport =
  case maybeReport of
  Nothing -> div [] []
  Just someReport ->
    tr [] [
      td[][text (Maybe.withDefault "<missing>" someReport.companyName)],
      td[][text (Round.round 2 (Maybe.withDefault 0.0 someReport.companyBalance))]
    ]

reportTable : List (Maybe CompanyReport) -> Html Msg
reportTable maybeReports =
  if (maybeReports == []) then
    div [] []
  else
    table [ class "table is-bordered is-striped is-fullwidth" ]
    ( List.concat
      [ [ thead []
          [ th [] [text "Company Name"]
          , th [] [text "Company Balance"]
          ]
        ]
      , List.map reportToRow maybeReports
      ]
    )



-- HTTP
type alias AllReportResponse = List (Maybe CompanyReport)

type alias CompanyReport =
  { reportId : Scalar.Id
  , companyName : Maybe String
  , companyBalance : Maybe Float
  }

type alias ResponseModel =
  RemoteData (Graphql.Http.Error (Maybe AllReportResponse)) (Maybe AllReportResponse)

makeRequest : Cmd Msg
makeRequest =
  reportQuery 
    |> Graphql.Http.queryRequest "http://localhost:8080/graphql"
    |> Graphql.Http.send (RemoteData.fromResult >> LoadReport)

reportQuery : SelectionSet (Maybe (List (Maybe CompanyReport))) RootQuery
reportQuery =
  Query.getAllReports reportSelection

reportSelection : SelectionSet CompanyReport ReportApi.Object.CompanyReport
reportSelection =
  SelectionSet.map3 CompanyReport
    CompanyReport.reportId
    CompanyReport.companyName
    CompanyReport.companyBalance

