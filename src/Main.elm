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
import Codecs exposing (AxisData(..), AxisDataType(..), IntermPostCountPerX, PushShiftData(..), PushShiftPostCount, PushShiftPostCountSubReddit, PushShiftResult(..), SnapshotResult(..), User(..), deserializeSnapShot, postCountDecoder, postCountSubRedditDecoder, pushShiftAggDecoder, serializeSnapShot, snapShotTimeFormatted)
import Color exposing (Color)
import Constants
import DateFormat
import Debug
import Dict
import Dict.Extra
import Hotkeys exposing (onEnterSend)
import Html exposing (Html, a, button, div, h3, i, input, text)
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
import Url.Parser as UrlParser
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
    | Failure String
    | Success PushShiftResult
    | Snapshot SnapshotResult


init : String -> ( Model, Cmd Msg )
init urlString =
    Url.fromString urlString
        |> Maybe.andThen extractSnapShotArgument
        |> Maybe.map deserializeSnapShot
        |> Maybe.map
            (\maybeSnapshotResult ->
                case maybeSnapshotResult of
                    Just (SnapshotResult user time data) ->
                        ( Snapshot (SnapshotResult user time data), Cmd.none )

                    Nothing ->
                        ( Failure "Bad snaphot data.", Cmd.none )
            )
        |> Maybe.withDefault ( InputUser <| User "", Cmd.none )


extractSnapShotArgument : Url.Url -> Maybe String
extractSnapShotArgument location =
    { location | path = "" }
        |> UrlParser.parse (UrlParser.query (UrlParserQuery.string "snapshot"))
        |> Maybe.withDefault Nothing


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
                    ( Failure "Error retriving data from the Pushshift API.", Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


inputName : User -> Html Msg
inputName (User user) =
    div [ Html.Attributes.class Pure.grid, Html.Attributes.class (Pure.unit [ "1", "1" ]) ] <|
        [ div [ Html.Attributes.class (Pure.unit [ "6", "24" ]) ] []
        , input
            [ Html.Attributes.autofocus True
            , Html.Attributes.class "input-reddit"
            , Html.Attributes.class (Pure.unit [ "8", "24" ])
            , placeholder "User to search for."
            , value user
            , onInput <| ChangeInput << (\u -> User u)
            , onEnterSend <| Search << (\u -> User u)
            ]
            []
        , div [ Html.Attributes.class (Pure.unit [ "1", "24" ]) ] []
        , button
            [ Html.Attributes.class Pure.button
            , Html.Attributes.class "button-reddit"
            , Html.Attributes.class (Pure.unit [ "3", "24" ])
            , onClick (Search <| User user)
            ]
            [ i [ Html.Attributes.class "fab", Html.Attributes.class "fa-reddit-alien", Html.Attributes.class "fa-2x" ] [] ]
        , div [ Html.Attributes.class (Pure.unit [ "6", "24" ]) ] []
        ]


view : Model -> Html Msg
view model =
    case model of
        InputUser (User user) ->
            div [ Html.Attributes.class (Pure.unit [ "5", "6" ]) ] []
                :: githubLinkElement
                :: [ inputName (User user) ]
                |> div [ Html.Attributes.class "content", Html.Attributes.class Pure.grid ]

        Loading ->
            div [ Html.Attributes.class Pure.grid, Html.Attributes.class "loader" ] []

        Failure message ->
            div [ Html.Attributes.class (Pure.unit [ "2", "3" ]) ] []
                :: homeElement
                :: githubLinkElement
                :: [ div [ Html.Attributes.class (Pure.unit [ "1", "1" ]), Html.Attributes.class "info-container" ] [ text message ] ]
                |> div [ Html.Attributes.class "content", Html.Attributes.class Pure.grid ]

        Success (PushShiftResult (User user) time pushShiftData) ->
            let
                allAxisData =
                    getAggregatedAxisData pushShiftData
            in
            div [ Html.Attributes.class (Pure.unit [ "1", "2" ]) ] []
                :: snapshotElement (SnapshotResult (User user) time allAxisData)
                :: homeElement
                :: githubLinkElement
                :: userInfoElement (User user) time
                :: [ chartsElement allAxisData ]
                |> div [ Html.Attributes.class "content", Html.Attributes.class Pure.grid ]

        Snapshot (SnapshotResult (User user) time allAxisData) ->
            div [ Html.Attributes.class (Pure.unit [ "2", "3" ]) ] []
                :: homeElement
                :: githubLinkElement
                :: userInfoElement (User user) time
                :: [ chartsElement allAxisData ]
                |> div [ Html.Attributes.class "content", Html.Attributes.class Pure.grid ]


snapshotElement : SnapshotResult -> Html Msg
snapshotElement (SnapshotResult (User user) time data) =
    serializeSnapShot (SnapshotResult (User user) time data)
        |> Maybe.map
            (\serialized ->
                [ a
                    [ Html.Attributes.class Pure.button
                    , Html.Attributes.class "button-links"
                    , Html.Attributes.href <| "?snapshot=" ++ serialized
                    , Html.Attributes.target "_blank"
                    ]
                    [ i [ Html.Attributes.class "fas", Html.Attributes.class "fa-share-alt", Html.Attributes.class "fa-2x" ] [] ]
                ]
                    |> div [ Html.Attributes.class (Pure.unit [ "1", "6" ]) ]
            )
        |> Maybe.withDefault (div [ Html.Attributes.class (Pure.unit [ "1", "6" ]) ] [])


homeElement : Html Msg
homeElement =
    [ a
        [ Html.Attributes.class Pure.button
        , Html.Attributes.class "button-links"
        , Html.Attributes.href <| "index.html"
        ]
        [ i [ Html.Attributes.class "fas", Html.Attributes.class "fa-home", Html.Attributes.class "fa-2x" ] [] ]
    ]
        |> div [ Html.Attributes.class (Pure.unit [ "1", "6" ]) ]


githubLinkElement : Html Msg
githubLinkElement =
    [ a
        [ Html.Attributes.class Pure.button
        , Html.Attributes.class "button-links"
        , Html.Attributes.href <| "https://github.com/DamienOReilly/reddit-status"
        , Html.Attributes.target "_blank"
        ]
        [ i [ Html.Attributes.class "fab", Html.Attributes.class "fa-github", Html.Attributes.class "fa-2x" ] [] ]
    ]
        |> div [ Html.Attributes.class (Pure.unit [ "1", "6" ]) ]


userInfoElement : User -> Time.Posix -> Html Msg
userInfoElement (User user) time =
    [ text <| "Statistics for user: "
    , a
        [ Html.Attributes.href <| Constants.redditUserUrl ++ user
        , Html.Attributes.target "_blank"
        ]
        [ text user ]
    , text <| " as of " ++ snapShotTimeFormatted time
    ]
        |> div [ Html.Attributes.class (Pure.unit [ "1", "1" ]), Html.Attributes.class "info-container" ]


chartsElement : List AxisDataType -> Html Msg
chartsElement axisData =
    chartConstructor axisData
        |> List.map
            (\chart ->
                div
                    [ Html.Attributes.class (Pure.unit [ "1" ])
                    , Html.Attributes.class (Pure.unit [ "lg", "1", "2" ])
                    ]
                    [ Chart.chart [] chart ]
            )
        |> div [ Html.Attributes.class Pure.grid ]


getContentCountPerSubReddit : List PushShiftPostCountSubReddit -> AxisData
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
                filtered ++ [ PushShiftPostCountSubReddit "All others" (totalSum - topSum) ]

            else
                filtered

        values =
            List.map (.count >> toFloat) final

        labels =
            List.map .subreddit final
    in
    AxisData labels values


getContentCountPerYear : List PushShiftPostCount -> AxisData
getContentCountPerYear count =
    let
        result =
            count
                |> List.filter (\x -> x.count > 0)
                |> List.map
                    (\x ->
                        PushShiftPostCount (toYear utc <| millisToPosix <| x.date * 1000) x.count
                    )
                |> Dict.Extra.groupBy .date
                >> Dict.map (\_ -> List.map .count >> List.sum)
                >> Dict.toList
                >> List.map (\( date, aggCount ) -> PushShiftPostCount date aggCount)

        values =
            List.map (.count >> toFloat) result

        labels =
            List.map (.date >> String.fromInt) result
    in
    AxisData labels values


getContentCountPerMonth : List PushShiftPostCount -> AxisData
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
                        IntermPostCountPerX
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


getAggregatedAxisData : List PushShiftData -> List AxisDataType
getAggregatedAxisData data =
    data
        |> List.map
            (\d ->
                case d of
                    RedditPostCount postCount ->
                        [ PostCountPerYear <| getContentCountPerYear postCount
                        , PostCountPerMonth <| getContentCountPerMonth postCount
                        ]

                    RedditSubmissionCount submissionCount ->
                        [ SubmissionCountPerYear <| getContentCountPerYear submissionCount
                        , SubmissionCountPerMonth <| getContentCountPerMonth submissionCount
                        ]

                    RedditPostCountPerSubReddit postCountPerSubReddit ->
                        [ PostCountPerSubReddit <| getContentCountPerSubReddit postCountPerSubReddit ]

                    RedditSubmissionCountPerSubReddit submissionCountPerSubReddit ->
                        [ SubmissionCountPerSubReddit <| getContentCountPerSubReddit submissionCountPerSubReddit ]
            )
        |> List.concat


makeBarOrLineChart : Chart.Type -> (AxisData -> String -> ChartCommon.PointProperty Color -> ChartCommon.PointProperty Color -> ChartData.Data) -> AxisData -> String -> ChartCommon.PointProperty Color -> ChartCommon.PointProperty Color -> String -> Chart.Chart
makeBarOrLineChart chartType chartConstructorFn axisData label colora color title =
    Chart.defaultChart chartType
        |> Chart.setData
            (chartConstructorFn axisData
                label
                colora
                color
            )
        |> Chart.setOptions (chartOptions title)


makePieChart : AxisData -> List Color -> String -> String -> Chart.Chart
makePieChart axisData colors label title =
    Chart.defaultChart Chart.Pie
        |> Chart.setData
            (constructPieChartData axisData
                label
                colors
            )
        |> Chart.setOptions (chartOptions title)


chartConstructor : List AxisDataType -> List Chart.Chart
chartConstructor data =
    data
        |> List.map
            (\d ->
                case d of
                    PostCountPerYear count ->
                        makeBarOrLineChart Chart.Bar constructBarChartData count "Posts" (ChartCommon.All Constants.colora2) (ChartCommon.All Constants.color2) "Posts per year"

                    PostCountPerMonth count ->
                        makeBarOrLineChart Chart.Line constructLineChartData count "Posts" (ChartCommon.All Constants.colora3) (ChartCommon.All Constants.color3) ("Posts per month (last " ++ String.fromInt Constants.lastNumberOfMonthsForLineGraph ++ ")")

                    SubmissionCountPerYear count ->
                        makeBarOrLineChart Chart.Bar constructBarChartData count "Submissions" (ChartCommon.All Constants.colora6) (ChartCommon.All Constants.color6) "Submissions per year"

                    SubmissionCountPerMonth count ->
                        makeBarOrLineChart Chart.Line constructLineChartData count "Submissions" (ChartCommon.All Constants.colora7) (ChartCommon.All Constants.color7) ("Submissions per month (last " ++ String.fromInt Constants.lastNumberOfMonthsForLineGraph ++ ")")

                    PostCountPerSubReddit count ->
                        makePieChart count Constants.borderColors "Submissions" ("Posts per subreddit (Top " ++ String.fromInt Constants.topSubreddits ++ ")")

                    SubmissionCountPerSubReddit count ->
                        makePieChart count Constants.borderColors "Submissions" ("Submissions per subreddit (Top " ++ String.fromInt Constants.topSubreddits ++ ")")
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
