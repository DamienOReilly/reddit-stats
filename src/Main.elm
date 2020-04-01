module Main exposing (main)

import Browser
import Chartjs.Chart as Chart
import Chartjs.Common as ChartCommon
import Chartjs.Data as ChartData
import Chartjs.DataSets.Bar as BarData
import Color
import Debug exposing (todo)
import Dict
import Dict.Extra exposing (groupBy)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (Decoder, field, int, map, map2, string)
import List exposing (length, map)
import Pure
import Task exposing (Task)
import Time exposing (millisToPosix, toYear, utc)
import Tuple exposing (first)


type User
    = User String


type Aggregator
    = Aggregator String


type PushShiftData
    = RedditPostCount (List PostCount)
    | RedditPostCountPerSubReddit (List PostCountSubReddit)


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


type BarChartData
    = BarChartData String (List String) (List Float)


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



-- view : Model -> Html Msg
-- view model =
--     case model of
--         TargetUser (User user) ->
--             div [ Html.Attributes.class Pure.grid ]
--                 [ input [ placeholder "User to search for.", value user, onInput <| ChangeInput << (\u -> User u) ] []
--                 , button [ Html.Attributes.class Pure.buttonPrimary, onClick (Search <| User user) ] [ text "Go!" ]
--                 ]
--         Loading ->
--             text "Getting stats."
--         Failure ->
--             text "Something bad happened."
--         Success pushShiftData ->
--             pushShiftData
--                 |> List.map
--                     (\data ->
--                         case data of
--                             RedditPostCount postCounts ->
--                                 let
--                                     result =
--                                         postCounts
--                                             |> List.filter (\x -> x.count > 0)
--                                             |> List.map
--                                                 (\x ->
--                                                     let
--                                                         year =
--                                                             toYear utc <| millisToPosix <| x.date * 1000
--                                                     in
--                                                     PostCount x.count year
--                                                 )
--                                             |> Dict.Extra.groupBy .date
--                                             >> Dict.map (\_ -> List.map .count >> List.sum)
--                                             >> Dict.toList
--                                             >> List.map (\( x, y ) -> PostCount y x)
--                                 in
--                                 let
--                                     values =
--                                         List.map (.count >> toFloat) result
--                                     labels =
--                                         List.map (.date >> String.fromInt) result
--                                     dataset =
--                                         BarData.defaultBarFromData "Posts per year" values
--                                             |> BarData.setBackgroundColor (ChartCommon.All Color.purple)
--                                 in
--                                 Chart.chart []
--                                     (Chart.defaultChart Chart.Bar
--                                         |> Chart.setData
--                                             (ChartData.dataFromLabels
--                                                 labels
--                                                 |> ChartData.addDataset (ChartData.BarData dataset)
--                                             )
--                                     )
--                             RedditPostCountPerSubReddit postCounts1 ->
--                                 text <| "Post counts: " ++ String.fromInt (length postCounts1)
--                     )
--                 |> div []


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


something : PushShiftData -> BarChartData
something data =
    case data of
        RedditPostCount postCounts ->
            let
                result =
                    postCounts
                        |> List.filter (\x -> x.count > 0)
                        |> List.map
                            (\x ->
                                let
                                    year =
                                        toYear utc <| millisToPosix <| x.date * 1000
                                in
                                PostCount x.count year
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
            BarChartData "Posts per year" labels values

        RedditPostCountPerSubReddit postCount ->
            BarChartData "TODO" ["One", "Two", "Three", "Four"] [1.0, 50.0, 30.0, 109.0]


chartConfig : List PushShiftData -> List Chart.Chart
chartConfig data =
    data
        |> List.map
            (\d ->
                Chart.defaultChart Chart.Bar
                    |> Chart.setData (constructChartData <| something d)
            )


constructChartData : BarChartData -> ChartData.Data
constructChartData (BarChartData title labels values) =
    let
        dataset =
            BarData.defaultBarFromData title values
                |> BarData.setBackgroundColor (ChartCommon.All Color.purple)
    in
    ChartData.dataFromLabels labels
        |> ChartData.addDataset (ChartData.BarData dataset)


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


getRedditStatistics : User -> Task Error (List PushShiftData)
getRedditStatistics user =
    let
        requests =
            [ getPushShiftData user (Aggregator "created_utc") postCountDecoder
                |> Task.map RedditPostCount
            , getPushShiftData user (Aggregator "subreddit") postCountSubRedditDecoder
                |> Task.map RedditPostCountPerSubReddit
            ]
    in
    Task.sequence requests
        |> Task.mapError HttpError


getPushShiftData : User -> Aggregator -> JD.Decoder a -> Task Http.Error (List a)
getPushShiftData (User user) (Aggregator aggregator) decoder =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://api.pushshift.io/reddit/search/comment?author=" ++ user ++ "&aggs=" ++ aggregator ++ "&frequency=day&size=0"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| pushShiftAggDecoder decoder aggregator
        , timeout = Nothing
        }


postCountDecoder : JD.Decoder PostCount
postCountDecoder =
    map2 PostCount
        (field "doc_count" int)
        (field "key" int)


postCountSubRedditDecoder : JD.Decoder PostCountSubReddit
postCountSubRedditDecoder =
    map2 PostCountSubReddit
        (field "doc_count" int)
        (field "key" string)


pushShiftAggDecoder : JD.Decoder a -> String -> Decoder (List a)
pushShiftAggDecoder decoder aggregator =
    JD.map identity (field "aggs" (field aggregator (JD.list decoder)))


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
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
