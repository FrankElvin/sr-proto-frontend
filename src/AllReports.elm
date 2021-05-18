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
  , statusText: Maybe String
  }
 
type ModelState
  = Waiting 
  | LoadFailure String
  | Loading Int
  | Success


init : () -> (Model, Cmd Msg)
init _ =
  (Model [] Nothing, makeRequest)


-- UPDATE
type Msg 
  = StartSearch 
  | LoadReport ResponseModel

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartSearch ->
      ( model, makeRequest )

    LoadReport responseModel -> 
      case responseModel of
        RemoteData.Loading ->
          ( { model | statusText = Just "Loading reports..." }, Cmd.none)

        RemoteData.Success data ->
          case data of
            Nothing ->
              ( { model | data = [], statusText = Just "Report not found" }, Cmd.none)
            Just reportList -> 
              ( { model | data = reportList,  statusText = Nothing }, Cmd.none)

        RemoteData.NotAsked ->
          ( { model | statusText = Just "Starting load of the report" },  Cmd.none )

        RemoteData.Failure error ->
          ( { model | statusText = Just "Failed to load the report" }, Cmd.none )



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Campaign report loader" ]
    , statusTextBar model.statusText
    , table [] (
        List.concat [ [
          thead [] [
            th [][text "CompanyName"],
            th [][text "CompanyBalance"]
          ]
          ],
          List.map reportToRow model.data
        ]
      )
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

