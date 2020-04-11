module Main exposing (main)

import Browser
import Chartjs.Chart as Chart
import Chartjs.Common as ChartCommon
import Chartjs.Data as ChartData
import Chartjs.DataSets.Bar as BarData
import Chartjs.DataSets.DoughnutAndPie as DoughnutAndPieData
import Chartjs.DataSets.Line as LineData
import Chartjs.Options as ChartOptions
import Chartjs.Options.Legend as ChartLegend
import Chartjs.Options.Title as ChartTitle
import Codecs exposing (PostCount, PostCountPerDay, PostCountSubReddit, PushShiftData(..), PushShiftResult(..), User(..), deserializeSnapShot, postCountDecoder, postCountSubRedditDecoder, pushShiftAggDecoder, serializeSnapShot)
import Color exposing (Color)
import Constants
import DateFormat
import Debug
import Dict
import Dict.Extra
import Hotkeys exposing (onEnterSend)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JsonDecode
import List
import List.Extra
import Maybe
import Pure
import Task exposing (Task)
import Time exposing (Month(..), millisToPosix, toYear, utc)
import Url
import Url.Parser as UrlParser exposing ((<?>))
import Url.Parser.Query as UrlParserQuery
import Utils exposing (descending, sortByWith)


type Aggregator
    = CreatedUTC
    | SubReddit


type Content
    = Comment
    | Submission


type Error
    = HttpError Http.Error


type Msg
    = GotResult (Result Error PushShiftResult)
    | ChangeInput User
    | Search User


type Model
    = InputUser User
    | Loading
    | Failure
    | Success PushShiftResult


type AxisData
    = AxisData (List String) (List Float)


init : String -> ( Model, Cmd Msg )
init urlString =
    Url.fromString urlString
        |> Maybe.map
            (\url ->
                case extractSnapShotArgument url of
                    Just (PushShiftResult user time data) ->
                        ( Success (PushShiftResult user time data), Cmd.none )

                    Nothing ->
                        ( InputUser <| User "", Cmd.none )
            )
        |> Maybe.withDefault ( InputUser <| User "", Cmd.none )


extractSnapShotArgument : Url.Url -> Maybe PushShiftResult
extractSnapShotArgument location =
    { location | path = "" }
        |> UrlParser.parse (UrlParser.query (UrlParserQuery.string "snapshot"))
        |> Maybe.withDefault Nothing
        |> Maybe.andThen deserializeSnapShot


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        ChangeInput user ->
            ( InputUser user, Cmd.none )

        Search (User user) ->
            if String.length user > 0 then
                ( Loading, Task.attempt GotResult <| getRedditStatistics <| User user )

            else
                ( InputUser <| User "", Cmd.none )

        GotResult result ->
            case result of
                Ok pushShiftResult ->
                    ( Success pushShiftResult, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


inputName : User -> Html Msg
inputName (User user) =
    div [ Html.Attributes.class Pure.grid ] <|
        [ input
            [ Html.Attributes.autofocus True
            , Html.Attributes.class "input-reddit"
            , placeholder "User to search for."
            , value user
            , onInput <| ChangeInput << (\u -> User u)
            , onEnterSend <| Search << (\u -> User u)
            ]
            []
        , button
            [ Html.Attributes.class Pure.button
            , Html.Attributes.class "button-reddit"
            , onClick (Search <| User user)
            ]
            [ text "Go!" ]
        ]

{-| Maybe.withDefault "" (serializeSnapShot (PushShiftResult user time pushShiftData))-} 

view : Model -> Html Msg
view model =
    case model of
        InputUser (User user) ->
            inputName (User user)

        Loading ->
            div [ Html.Attributes.class Pure.grid, Html.Attributes.class "loader" ] []

        Failure ->
            text "Something bad happened."
        Success (PushShiftResult user time pushShiftData) ->
            inputName user
                :: div [] [ text  "" ]
                :: [ chartConstructor pushShiftData
                        |> List.map
                            (\chart ->
                                div
                                    [ Html.Attributes.class (Pure.unit [ "1" ])
                                    , Html.Attributes.class (Pure.unit [ "md", "1", "2" ])
                                    ]
                                    [ Chart.chart [] chart ]
                            )
                        |> div [ Html.Attributes.class Pure.grid ]
                   ]
                |> div []


getContentCountPerSubReddit : List PostCountSubReddit -> AxisData
getContentCountPerSubReddit count =
    let
        totalSum =
            List.map .count count |> List.sum

        filtered =
            count
                |> List.filter (\x -> x.count > 0)
                |> sortByWith .count descending
                |> List.take Constants.topSubreddits

        topSum =
            List.map .count filtered |> List.sum

        difference =
            totalSum - topSum

        final =
            if difference > 0 then
                filtered ++ [ PostCountSubReddit "All others" (totalSum - topSum) ]

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
                        PostCount (toYear utc <| millisToPosix <| x.date * 1000) x.count
                    )
                |> Dict.Extra.groupBy .date
                >> Dict.map (\_ -> List.map .count >> List.sum)
                >> Dict.toList
                >> List.map (\( date, aggCount ) -> PostCount date aggCount)

        values =
            List.map (.count >> toFloat) result

        labels =
            List.map (.date >> String.fromInt) result
    in
    AxisData labels values


getContentCountPerMonth : List PostCount -> AxisData
getContentCountPerMonth count =
    let
        result =
            count
                |> sortByWith .date descending
                |> List.take Constants.lastNumberOfMonthsForLineGraph
                |> List.map
                    (\x ->
                        let
                            datePosix =
                                millisToPosix <| x.date * 1000
                        in
                        PostCountPerDay
                            (DateFormat.format
                                [ DateFormat.monthNameAbbreviated
                                , DateFormat.text " "
                                , DateFormat.yearNumber
                                ]
                                utc
                                datePosix
                            )
                            x.count
                    )
                |> List.reverse

        values =
            List.map (.count >> toFloat) result

        labels =
            List.map .date result
    in
    AxisData labels values


chartConstructor : List PushShiftData -> List Chart.Chart
chartConstructor data =
    data
        |> List.map
            (\d ->
                case d of
                    RedditPostCount postCount ->
                        [ Chart.defaultChart Chart.Bar
                            |> Chart.setData
                                (constructBarChartData (getContentCountPerYear postCount)
                                    "Posts"
                                    (ChartCommon.All Constants.colora2)
                                    (ChartCommon.All Constants.color2)
                                )
                            |> Chart.setOptions (chartOptions "Posts per year")
                        , Chart.defaultChart Chart.Line
                            |> Chart.setData
                                (constructLineChartData (getContentCountPerMonth postCount)
                                    "Posts"
                                    (ChartCommon.All Constants.colora3)
                                    (ChartCommon.All Constants.color3)
                                )
                            |> Chart.setOptions (chartOptions <| "Posts per month (last " ++ String.fromInt Constants.lastNumberOfMonthsForLineGraph ++ ")")
                        ]

                    RedditSubmissionCount submissionCount ->
                        [ Chart.defaultChart Chart.Bar
                            |> Chart.setData
                                (constructBarChartData (getContentCountPerYear submissionCount)
                                    "Submissions"
                                    (ChartCommon.All Constants.colora6)
                                    (ChartCommon.All Constants.color6)
                                )
                            |> Chart.setOptions (chartOptions "Submissions per year")
                        , Chart.defaultChart Chart.Line
                            |> Chart.setData
                                (constructLineChartData (getContentCountPerMonth submissionCount)
                                    "Submissions"
                                    (ChartCommon.All Constants.colora7)
                                    (ChartCommon.All Constants.color7)
                                )
                            |> Chart.setOptions (chartOptions <| "Submissions per month (last " ++ String.fromInt Constants.lastNumberOfMonthsForLineGraph ++ ")")
                        ]

                    RedditPostCountPerSubReddit postCountPerSubReddit ->
                        [ Chart.defaultChart Chart.Pie
                            |> Chart.setData
                                (constructPieChartData
                                    (getContentCountPerSubReddit postCountPerSubReddit)
                                    "Posts"
                                    Constants.borderColors
                                )
                            |> Chart.setOptions (chartOptions <| "Posts per subreddit (Top " ++ String.fromInt Constants.topSubreddits ++ ")")
                        ]

                    RedditSubmissionCountPerSubReddit submissionCountPerSubReddit ->
                        [ Chart.defaultChart Chart.Pie
                            |> Chart.setData
                                (constructPieChartData
                                    (getContentCountPerSubReddit submissionCountPerSubReddit)
                                    "Submissions"
                                    Constants.borderColors
                                )
                            |> Chart.setOptions (chartOptions <| "Submissions per subreddit (Top " ++ String.fromInt Constants.topSubreddits ++ ")")
                        ]
            )
        |> List.concat


chartOptions : String -> ChartOptions.Options
chartOptions title =
    ChartOptions.defaultOptions
        |> ChartOptions.setTitle
            (ChartTitle.defaultTitle
                |> ChartTitle.setText
                    title
                |> ChartTitle.setDisplay True
            )
        |> ChartOptions.setLegend
            (ChartLegend.defaultLegend
                |> ChartLegend.setPosition ChartCommon.Bottom
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


constructLineChartData : AxisData -> String -> ChartCommon.PointProperty Color -> ChartCommon.PointProperty Color -> ChartData.Data
constructLineChartData (AxisData labels values) label bgColor bdColor =
    let
        dataSet =
            LineData.defaultLineFromData label values
                |> LineData.setBackgroundColor bgColor
                |> LineData.setBorderColor bdColor
                |> LineData.setBorderWidth (ChartCommon.All 1)
    in
    ChartData.dataFromLabels labels
        |> ChartData.addDataset (ChartData.LineData dataSet)


main : Program String Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


getRedditStatistics : User -> Task Error PushShiftResult
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
    Time.now
        |> Task.andThen
            (\time ->
                Task.sequence requests
                    |> Task.map (\results -> PushShiftResult (User user) time results)
                    |> Task.mapError HttpError
            )


getPushShiftData : User -> Aggregator -> Content -> JsonDecode.Decoder a -> Task Http.Error (List a)
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
        , url = "https://" ++ Constants.pushShiftServer ++ "/reddit/search/" ++ contentType ++ "?author=" ++ user ++ "&aggs=" ++ aggregatorType ++ "&frequency=month&size=0"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| pushShiftAggDecoder decoder aggregatorType
        , timeout = Just Constants.httpTimeout
        }


handleJsonResponse : JsonDecode.Decoder a -> Http.Response String -> Result Http.Error a
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
            case JsonDecode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result
