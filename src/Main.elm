module Main exposing (main)

import Browser
import ChartUtils exposing (chartConstructor, getAggregatedAxisData)
import Chartjs.Chart as Chart
import Codecs exposing (AxisData(..), AxisDataType(..), PushShiftData(..), PushShiftResult(..), SnapshotResult(..), User(..), countDecoder, countSubRedditDecoder, deserializeSnapShot, pushShiftAggDecoder, serializeSnapShot, snapShotTimeFormatted)
import Constants
import Hotkeys exposing (onEnterSend)
import Html exposing (Html, a, button, div, h1, i, input, p, span, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JsonDecode
import List
import Maybe
import Pure
import Task exposing (Task)
import Time exposing (Month(..))
import Url
import Url.Parser as UrlParser
import Url.Parser.Query as UrlParserQuery


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


inputName : User -> Html Msg
inputName (User user) =
    div [ Html.Attributes.class "splash", Html.Attributes.class (Pure.unit [ "1", "1" ]) ]
        [ h1 [ Html.Attributes.class "splash-head" ] [ text "Reddit User Statistics" ]
        , p [ Html.Attributes.class "splash-subhead" ] [ text "All data retrieved from the PushShift API is processed directly on the browser. No interim server is used." ]
        , div [ Html.Attributes.class "input-container", Html.Attributes.class Pure.grid, Html.Attributes.class (Pure.unit [ "1", "1" ]) ] <|
            [ input
                [ Html.Attributes.autofocus True
                , Html.Attributes.class "input-reddit"
                , placeholder "User to get stats on..."
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
                [ i [ Html.Attributes.class "fab", Html.Attributes.class "fa-reddit-alien", Html.Attributes.class "fa-2x" ] [] ]
            ]
        ]


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
        , Html.Attributes.href <| Constants.githubRepoUrl
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


getRedditStatistics : User -> Task Error PushShiftResult
getRedditStatistics (User user) =
    let
        u =
            User (String.replace "/u/" "" user |> String.replace "u/" "")

        requests =
            [ getPushShiftData u SubReddit Comment countSubRedditDecoder
                |> Task.map RedditCommentCountPerSubReddit
            , getPushShiftData u SubReddit Submission countSubRedditDecoder
                |> Task.map RedditSubmissionCountPerSubReddit
            , getPushShiftData u CreatedUTC Comment countDecoder
                |> Task.map RedditCommentCount
            , getPushShiftData u CreatedUTC Submission countDecoder
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


extractSnapShotArgument : Url.Url -> Maybe String
extractSnapShotArgument location =
    { location | path = "" }
        |> UrlParser.parse (UrlParser.query (UrlParserQuery.string "snapshot"))
        |> Maybe.withDefault Nothing


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


view : Model -> Html Msg
view model =
    case model of
        InputUser (User user) ->
            div [ Html.Attributes.class (Pure.unit [ "5", "6" ]) ] []
                :: githubLinkElement
                :: [ inputName (User user) ]
                |> div [ Html.Attributes.class "content", Html.Attributes.class Pure.grid ]

        Loading ->
            div [ Html.Attributes.class Pure.grid, Html.Attributes.class "loader-container" ]
                [ span [ Html.Attributes.class "loader" ] []
                ]

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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
