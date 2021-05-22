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
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL
type alias Model =
  { data: List (Maybe CompanyReport)
  , filteredData: List (Maybe CompanyReport)
  , searchFilter: String
  , statusText: Maybe String
  }
 
init : () -> (Model, Cmd Msg)
init _ =
  (Model [] [] "" Nothing, makeRequest)



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
      if (String.isEmpty filter) then
        ( { model | searchFilter = filter, filteredData=model.data }, Cmd.none)
      else 
        ( { model |
            searchFilter = filter
          , filteredData = List.filter (isMatchingFilter filter) model.data
          }
        , Cmd.none)

    LoadReport responseModel -> 
      case responseModel of
        RemoteData.Loading ->
          ( { model | statusText = Just "Loading reports..." }, Cmd.none)

        RemoteData.Success data ->
          case data of
            Nothing ->
              ( { model | data = [], statusText = Just "No repors downloaded" }, Cmd.none)
            Just reportList -> 
              ( { model | data = reportList, filteredData = reportList, statusText = Nothing }, Cmd.none)

        RemoteData.NotAsked ->
          ( { model | statusText = Just "Loading reports..." },  Cmd.none )

        RemoteData.Failure error ->
          ( { model | statusText = Just "Failed to load reports" }, Cmd.none )

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
view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Campaign report loader" ]
    , div [] [
        input [ placeholder "Report search filter", onInput FilterData ] []
      , button [ onClick StartSearch ] [ text "Update data" ]
      ]
    , statusTextBar model.statusText
    , reportTable model.filteredData
    ]

statusTextBar : Maybe String -> Html Msg
statusTextBar statusText = 
  case statusText of 
    Nothing -> div [] [] 
    Just someString -> div [] [text someString] 

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
    table [ ]
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

