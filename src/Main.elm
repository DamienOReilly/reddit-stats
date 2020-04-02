module Main exposing (main)

import Browser
import Chartjs.Chart as Chart
import Chartjs.Common as ChartCommon
import Chartjs.Data as ChartData
import Chartjs.DataSets.Bar as BarData
import Color exposing (rgb, rgba)
import Debug exposing (todo)
import Dict
import Dict.Extra exposing (groupBy)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (Decoder, field, int, map, map2, string)
import List exposing (length, map)
import List.Extra
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


type ChartData
    = ChartData String (List String) (List Float)


borderColors =
    [ rgb 0.65 0.807 0.89
    , rgb 0.121 0.47 0.705
    , rgb 0.698 0.874 0.541
    , rgb 0.2 0.627 0.172
    , rgb 0.984 0.603 0.6
    , rgb 0.89 0.101 0.109
    , rgb 0.992 0.749 0.435
    , rgb 1 0.498 0
    , rgb 0.792 0.698 0.839
    , rgb 0.415 0.239 0.603
    , rgb 0.694 0.349 0.156
    ]


backgroundColors =
    [ rgba 0.65 0.807 0.89 0.2
    , rgba 0.121 0.47 0.705 0.2
    , rgba 0.698 0.874 0.541 0.2
    , rgba 0.2 0.627 0.172 0.2
    , rgba 0.984 0.603 0.6 0.2
    , rgba 0.89 0.101 0.109 0.2
    , rgba 0.992 0.749 0.435 0.2
    , rgba 1 0.498 0 0.2
    , rgba 0.792 0.698 0.839 0.2
    , rgba 0.415 0.239 0.603 0.2
    , rgba 0.694 0.349 0.156 0.2
    ]


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


descending : comparable -> comparable -> Order
descending a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


orderBy : (a -> comparable) -> (comparable -> comparable -> Order) -> a -> a -> Order
orderBy accessor orderFunc a b =
    orderFunc (accessor a) (accessor b)


sortByWith : (a -> comparable) -> (comparable -> comparable -> Order) -> List a -> List a
sortByWith accessor sortFunc list =
    List.sortWith (orderBy accessor sortFunc) list


getAxisData : PushShiftData -> ChartData
getAxisData data =
    case data of
        RedditPostCount postCounts ->
            let
                result =
                    postCounts
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
            ChartData "Posts per year" labels values

        RedditPostCountPerSubReddit postCounts ->
            let
                totalSum =
                    List.map .count postCounts |> List.sum

                filtered =
                    postCounts
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
            ChartData "Posts per subreddit (Top 10)" labels values


chartConfig : List PushShiftData -> List Chart.Chart
chartConfig data =
    data
        |> List.map
            (\d ->
                Chart.defaultChart Chart.Doughnut
                    |> Chart.setData (constructChartData <| getAxisData d)
            )


constructChartData : ChartData -> ChartData.Data
constructChartData (ChartData title labels values) =
    let
        bgColors =
            List.Extra.cycle (length values) backgroundColors

        bdColors =
            List.Extra.cycle (length values) borderColors

        dataset =
            BarData.defaultBarFromData title values
                |> BarData.setBackgroundColor (ChartCommon.PerPoint bgColors)
                |> BarData.setBorderColor (ChartCommon.PerPoint bdColors)
                |> BarData.setBorderWidth (ChartCommon.All 1)
    in
    ChartData.dataFromLabels labels
        |> ChartData.addDataset (ChartData.BarData dataset)


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


getRedditStatistics : User -> Task Error (List PushShiftData)
getRedditStatistics (User user) =
    let
        u =
            User <| (String.replace "/u/" "" user |> String.replace "u/" "")

        requests =
            [ getPushShiftData u (Aggregator "created_utc") postCountDecoder
                |> Task.map RedditPostCount
            , getPushShiftData u (Aggregator "subreddit") postCountSubRedditDecoder
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
