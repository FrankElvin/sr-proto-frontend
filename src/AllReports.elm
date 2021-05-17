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
  { textFilter: String
  , filter: Maybe Int
  , data: List (Maybe CompanyReport)
  , statusText: Maybe String
  }
 
type ModelState
  = Waiting 
  | LoadFailure String
  | Loading Int
  | Success


init : () -> (Model, Cmd Msg)
init _ =
  (Model  "" Nothing [] (Just "Specify the filter"), Cmd.none)


-- UPDATE
type Msg 
  = SaveSearchFilter String 
  | StartSearch 
  | LoadReport ResponseModel

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SaveSearchFilter filter ->
      if filter == "" then 
        ( { model | filter = Nothing, data = [], statusText = Just "Specify the filter" }, Cmd.none)
      else 
        case String.toInt filter of
          Nothing ->
            ( { model | filter = Nothing, data = [], statusText = Just "Incorrect filter (should be int)" }, Cmd.none)
          Just someNumber ->
            ( { model | filter = Just someNumber, statusText = Nothing }, Cmd.none)

    StartSearch ->
      case model.filter of
        Nothing ->
          ( model, Cmd.none )
        Just reportId ->
          ( model, makeRequest )

    LoadReport responseModel -> 
      case responseModel of
        RemoteData.Loading ->
          ( { model | statusText = Just "Loading report..." }, Cmd.none)

        RemoteData.Success data ->
          case data of
            Nothing ->
              ( { model | data = [], statusText = Just "Report not found" }, Cmd.none)
            Just reportList -> 
              -- ( { model | data = Just [Just companyReport] , statusText = Just "Loaded some company report" }, Cmd.none)
              ( { model | data = reportList,  statusText = Just "Loaded some company report" }, Cmd.none)

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
    , input [ placeholder "Id of report", onInput SaveSearchFilter ] []
    , searchButton model.filter
    , statusTextBar model.statusText
    , displayReportTable model
    ]

statusTextBar : Maybe String -> Html Msg
statusTextBar statusText = 
  case statusText of 
    Nothing -> div [] [] 
    Just someString -> div [] [text someString] 

searchButton : Maybe Int -> Html Msg
searchButton filter = 
  case filter of
    Nothing ->
      button [ onClick StartSearch, disabled True ] [ text "Search" ]
    Just _ -> 
      button [ onClick StartSearch, disabled False ] [ text "Search" ]
    

displayReportTable : Model -> Html Msg
displayReportTable model =
  div [] (List.map displayReport model.data)


displayReport : Maybe CompanyReport -> Html Msg
displayReport report =
  case report of
  Nothing -> pre [] []
  Just someReport ->
    pre [] [
      text ("Name: " ++ Maybe.withDefault "<missing>" someReport.companyName ++ 
            "\nBalance: " ++  Round.round 2 (Maybe.withDefault 0.0 someReport.companyBalance))
    ]



-- HTTP
type alias MyResponse = Maybe CompanyReport

type alias CompanyReport =
  { companyName : Maybe String
  , companyBalance : Maybe Float
  }

type alias ResponseModel =
  RemoteData (Graphql.Http.Error (Maybe (List MyResponse))) (Maybe (List MyResponse))

-- , data: Maybe (List (Maybe CompanyReport))
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
  SelectionSet.map2 CompanyReport
    CompanyReport.companyName
    CompanyReport.companyBalance

