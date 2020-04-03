module Main exposing (main)

import Browser
import Chartjs.Chart as Chart
import Chartjs.Common as ChartCommon
import Chartjs.Data as ChartData
import Chartjs.DataSets.Bar as BarData
import Chartjs.DataSets.DoughnutAndPie as DoughnutAndPieData
import Chartjs.Options as ChartOptions
import Chartjs.Options.Title as ChartTitle
import Color exposing (Color)
import Constants
import Debug exposing (todo)
import Dict
import Dict.Extra
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import List
import List.Extra
import Pure
import Task exposing (Task)
import Time exposing (millisToPosix, toYear, utc)
import Utils exposing (descending, sortByWith)


type User
    = User String


type Aggregator
    = CreatedUTC
    | SubReddit


type Content
    = Comment
    | Submission


type PushShiftData
    = RedditPostCountPerSubReddit (List PostCountSubReddit)
    | RedditSubmissionCountPerSubReddit (List PostCountSubReddit)
    | RedditPostCount (List PostCount)
    | RedditSubmissionCount (List PostCount)


type alias PostCount =
    { count : Int
    , date : Int
    }


type alias PostCountSubReddit =
    { count : Int
    , subreddit : String
    }


type Error
    = HttpError Http.Error


type Msg
    = GotResult (Result Error (List PushShiftData))
    | ChangeInput User
    | Search User


type Model
    = TargetUser User
    | Loading
    | Failure
    | Success (List PushShiftData)


type AxisData
    = AxisData (List String) (List Float)


init : () -> ( Model, Cmd Msg )
init _ =
    ( TargetUser <| User "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        ChangeInput user ->
            ( TargetUser user, Cmd.none )

        Search user ->
            ( Loading, Task.attempt GotResult <| getRedditStatistics user )

        GotResult result ->
            case result of
                Ok postCounts ->
                    ( Success postCounts, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        TargetUser (User user) ->
            div [ Html.Attributes.class Pure.grid ]
                [ input [ placeholder "User to search for.", value user, onInput <| ChangeInput << (\u -> User u) ] []
                , button [ Html.Attributes.class Pure.buttonPrimary, onClick (Search <| User user) ] [ text "Go!" ]
                ]

        Loading ->
            text "Getting stats."

        Failure ->
            text "Something bad happened."

        Success pushShiftData ->
            chartConfig pushShiftData
                |> List.map (\x -> Chart.chart [] x)
                |> div []


getAxisData : PushShiftData -> AxisData
getAxisData data =
    case data of
        RedditPostCount count ->
            getContentCountPerYear count

        RedditSubmissionCount count ->
            getContentCountPerYear count

        RedditPostCountPerSubReddit count ->
            getContentCountPerSubReddit count

        RedditSubmissionCountPerSubReddit count ->
            getContentCountPerSubReddit count


getContentCountPerSubReddit : List PostCountSubReddit -> AxisData
getContentCountPerSubReddit count =
    let
        totalSum =
            List.map .count count |> List.sum

        filtered =
            count
                |> List.filter (\x -> x.count > 0)
                |> sortByWith .count descending
                |> List.take 10

        topSum =
            List.map .count filtered |> List.sum

        difference =
            totalSum - topSum

        final =
            if difference > 0 then
                filtered ++ [ PostCountSubReddit (totalSum - topSum) "All others" ]

            else
                filtered

        values =
            List.map (.count >> toFloat) final

        labels =
            List.map .subreddit final
    in
    AxisData labels values


getContentCountPerYear : List PostCount -> AxisData
getContentCountPerYear count =
    let
        result =
            count
                |> List.filter (\x -> x.count > 0)
                |> List.map
                    (\x ->
                        PostCount x.count <| toYear utc <| millisToPosix <| x.date * 1000
                    )
                |> Dict.Extra.groupBy .date
                >> Dict.map (\_ -> List.map .count >> List.sum)
                >> Dict.toList
                >> List.map (\( x, y ) -> PostCount y x)

        values =
            List.map (.count >> toFloat) result

        labels =
            List.map (.date >> String.fromInt) result
    in
    AxisData labels values


chartConfig : List PushShiftData -> List Chart.Chart
chartConfig data =
    data
        |> List.map
            (\d ->
                case d of
                    RedditPostCount _ ->
                        Chart.defaultChart Chart.Bar
                            |> Chart.setData (constructBarChartData (getAxisData d) "Posts" (ChartCommon.All Constants.colora2) (ChartCommon.All Constants.color2))
                            |> Chart.setOptions (chartOptions "Posts per year")

                    RedditSubmissionCount _ ->
                        Chart.defaultChart Chart.Bar
                            |> Chart.setData (constructBarChartData (getAxisData d) "Submissions" (ChartCommon.All Constants.colora6) (ChartCommon.All Constants.color6))
                            |> Chart.setOptions (chartOptions "Submissions per year")

                    RedditPostCountPerSubReddit _ ->
                        Chart.defaultChart Chart.Pie
                            |> Chart.setData (constructPieChartData (getAxisData d) "Posts" Constants.borderColors)
                            |> Chart.setOptions (chartOptions "Posts per subreddit (Top 10)")

                    RedditSubmissionCountPerSubReddit _ ->
                        Chart.defaultChart Chart.Pie
                            |> Chart.setData (constructPieChartData (getAxisData d) "Submissions" Constants.borderColors)
                            |> Chart.setOptions (chartOptions "Submissions per subreddit (Top 10)")
            )


chartOptions : String -> ChartOptions.Options
chartOptions title =
    ChartOptions.defaultOptions
        |> ChartOptions.setTitle
            (ChartTitle.defaultTitle
                |> ChartTitle.setText
                    title
                |> ChartTitle.setDisplay True
            )


constructPieChartData : AxisData -> String -> List Color -> ChartData.Data
constructPieChartData (AxisData labels values) label colors =
    let
        colorsCycled =
            List.Extra.cycle (List.length values) colors

        dataSet =
            DoughnutAndPieData.defaultPieFromData label values
                |> DoughnutAndPieData.setBackgroundColor (ChartCommon.PerPoint colorsCycled)
    in
    ChartData.dataFromLabels labels
        |> ChartData.addDataset (ChartData.PieData dataSet)


constructBarChartData : AxisData -> String -> ChartCommon.PointProperty Color -> ChartCommon.PointProperty Color -> ChartData.Data
constructBarChartData (AxisData labels values) label bgColor bdColor =
    let
        dataSet =
            BarData.defaultBarFromData label values
                |> BarData.setBackgroundColor bgColor
                |> BarData.setBorderColor bdColor
                |> BarData.setBorderWidth (ChartCommon.All 1)
    in
    ChartData.dataFromLabels labels
        |> ChartData.addDataset (ChartData.BarData dataSet)


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


getRedditStatistics : User -> Task Error (List PushShiftData)
getRedditStatistics (User user) =
    let
        u =
            User (String.replace "/u/" "" user |> String.replace "u/" "")

        requests =
            [ getPushShiftData u SubReddit Comment postCountSubRedditDecoder
                |> Task.map RedditPostCountPerSubReddit

            , getPushShiftData u SubReddit Submission postCountSubRedditDecoder
                |> Task.map RedditSubmissionCountPerSubReddit
            , getPushShiftData u CreatedUTC Comment postCountDecoder
                |> Task.map RedditPostCount

            , getPushShiftData u CreatedUTC Submission postCountDecoder
                |> Task.map RedditSubmissionCount
            ]
    in
    Task.sequence requests
        |> Task.mapError HttpError


getPushShiftData : User -> Aggregator -> Content -> JD.Decoder a -> Task Http.Error (List a)
getPushShiftData (User user) aggregator content decoder =
    let
        aggregatorType =
            case aggregator of
                CreatedUTC ->
                    "created_utc"

                SubReddit ->
                    "subreddit"

        contentType =
            case content of
                Comment ->
                    "comment"

                Submission ->
                    "submission"
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://api.pushshift.io/reddit/search/" ++ contentType ++ "?author=" ++ user ++ "&aggs=" ++ aggregatorType ++ "&frequency=day&size=0"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| pushShiftAggDecoder decoder aggregatorType
        , timeout = Nothing
        }


postCountDecoder : JD.Decoder PostCount
postCountDecoder =
    JD.map2 PostCount
        (JD.field "doc_count" JD.int)
        (JD.field "key" JD.int)


postCountSubRedditDecoder : JD.Decoder PostCountSubReddit
postCountSubRedditDecoder =
    JD.map2 PostCountSubReddit
        (JD.field "doc_count" JD.int)
        (JD.field "key" JD.string)


pushShiftAggDecoder : JD.Decoder a -> String -> JD.Decoder (List a)
pushShiftAggDecoder decoder aggregator =
    JD.map identity (JD.field "aggs" (JD.field aggregator (JD.list decoder)))


handleJsonResponse : JD.Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case JD.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result
