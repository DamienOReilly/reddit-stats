module Codecs exposing (AxisData(..), AxisDataType(..), PostCount, PostCountPerX, PostCountSubReddit, PushShiftData(..), PushShiftResult(..), User(..), deserializeSnapShot, postCountDecoder, postCountSubRedditDecoder, pushShiftAggDecoder, serializeSnapShot, snapShotTimeFormatted)

import Base64
import Bytes
import Bytes.Decode as BytesDecode
import Bytes.Encode as BytesEncode
import DateFormat exposing (amPmLowercase, dayOfMonthSuffix, format, hourNumber, minuteFixed, monthNameAbbreviated, text, yearNumber)
import Flate
import Json.Decode as JsonDecode exposing (Decoder)
import Json.Encode as JsonEncode
import LZ77
import Time exposing (Posix(..))
import UrlBase64


type User
    = User String


type PushShiftData
    = RedditPostCountPerSubReddit (List PostCountSubReddit)
    | RedditSubmissionCountPerSubReddit (List PostCountSubReddit)
    | RedditPostCount (List PostCount)
    | RedditSubmissionCount (List PostCount)


type PushShiftResult
    = PushShiftResult User Time.Posix (List PushShiftData)


type AxisData
    = AxisData (List String) (List Float)


type AxisDataType
    = PostCountPerYear AxisData
    | SubmissionCountPerYear AxisData
    | PostCountPerSubReddit AxisData
    | SubmissionCountPerReddit AxisData
    | PostCountPerMonth AxisData
    | SubmissionCountPerMonth AxisData


type alias PostCount =
    { date : Int
    , count : Int
    }


type alias PostCountPerX =
    { date : String
    , count : Int
    }


type alias PostCountSubReddit =
    { subreddit : String
    , count : Int
    }


postCountDecoder : Decoder PostCount
postCountDecoder =
    JsonDecode.map2 PostCount
        (JsonDecode.field "key" JsonDecode.int)
        (JsonDecode.field "doc_count" JsonDecode.int)


postCountEncoder : PostCount -> JsonEncode.Value
postCountEncoder postCount =
    JsonEncode.object
        [ ( "key", JsonEncode.int postCount.date )
        , ( "doc_count", JsonEncode.int postCount.count )
        ]


postCountSubRedditDecoder : Decoder PostCountSubReddit
postCountSubRedditDecoder =
    JsonDecode.map2 PostCountSubReddit
        (JsonDecode.field "key" JsonDecode.string)
        (JsonDecode.field "doc_count" JsonDecode.int)


postCountSubRedditEncoder : PostCountSubReddit -> JsonEncode.Value
postCountSubRedditEncoder postCountSubReddit =
    JsonEncode.object
        [ ( "key", JsonEncode.string postCountSubReddit.subreddit )
        , ( "doc_count", JsonEncode.int postCountSubReddit.count )
        ]


pushShiftAggDecoder : Decoder a -> String -> Decoder (List a)
pushShiftAggDecoder decoder aggregator =
    JsonDecode.map identity (JsonDecode.field "aggs" (JsonDecode.field aggregator (JsonDecode.list decoder)))


pushShiftAggEncoder : (a -> JsonEncode.Value) -> List a -> String -> JsonEncode.Value
pushShiftAggEncoder encoderFn pushShiftData aggregator =
    JsonEncode.object
        [ ( "aggs"
          , JsonEncode.object
                [ ( aggregator, JsonEncode.list encoderFn pushShiftData )
                ]
          )
        ]


pushShiftEncoder : List PushShiftData -> JsonEncode.Value
pushShiftEncoder pushShiftData =
    pushShiftData
        |> List.map
            (\data ->
                case data of
                    RedditPostCount count ->
                        ( "post_count", pushShiftAggEncoder postCountEncoder count "created_utc" )

                    RedditSubmissionCount count ->
                        ( "submission_count", pushShiftAggEncoder postCountEncoder count "created_utc" )

                    RedditPostCountPerSubReddit count ->
                        ( "post_count_per_subreddit", pushShiftAggEncoder postCountSubRedditEncoder count "subreddit" )

                    RedditSubmissionCountPerSubReddit count ->
                        ( "submission_count_per_subreddit", pushShiftAggEncoder postCountSubRedditEncoder count "subreddit" )
            )
        |> JsonEncode.object


{-| Would like to now if its possible to do this without the intermediate type aliases
-}
type alias IntermediatePushShiftData =
    { postCountPerSubReddit : List PostCountSubReddit
    , submissionCountPerSubReddit : List PostCountSubReddit
    , postCount : List PostCount
    , submissionCount : List PostCount
    }


pushShiftDecoder : Decoder (List PushShiftData)
pushShiftDecoder =
    JsonDecode.map4 IntermediatePushShiftData
        (JsonDecode.field "post_count_per_subreddit" (pushShiftAggDecoder postCountSubRedditDecoder "subreddit"))
        (JsonDecode.field "submission_count_per_subreddit" (pushShiftAggDecoder postCountSubRedditDecoder "subreddit"))
        (JsonDecode.field "post_count" (pushShiftAggDecoder postCountDecoder "created_utc"))
        (JsonDecode.field "submission_count" (pushShiftAggDecoder postCountDecoder "created_utc"))
        |> JsonDecode.map
            (\x ->
                [ RedditPostCountPerSubReddit x.postCountPerSubReddit
                , RedditSubmissionCountPerSubReddit x.submissionCountPerSubReddit
                , RedditPostCount x.postCount
                , RedditSubmissionCount x.submissionCount
                ]
            )


snapShotEncoder : PushShiftResult -> JsonEncode.Value
snapShotEncoder (PushShiftResult (User user) time pushShiftData) =
    JsonEncode.object
        [ ( "user", JsonEncode.string user )
        , ( "snapshot_time", JsonEncode.int <| Time.posixToMillis time )
        , ( "data", pushShiftEncoder pushShiftData )
        ]


type alias IntermediatePushShiftResult =
    { user : String
    , snapShotTime : Int
    , data : List PushShiftData
    }


snapShotDecoder : Decoder PushShiftResult
snapShotDecoder =
    JsonDecode.map3 IntermediatePushShiftResult
        (JsonDecode.field "user" JsonDecode.string)
        (JsonDecode.field "snapshot_time" JsonDecode.int)
        (JsonDecode.field "data" pushShiftDecoder)
        |> JsonDecode.map
            (\x -> PushShiftResult (User x.user) (Time.millisToPosix x.snapShotTime) x.data)


deserializeSnapShot : String -> Maybe PushShiftResult
deserializeSnapShot input =
    UrlBase64.decode base64ToPushShiftResult input
        |> Result.toMaybe


base64ToPushShiftResult : String -> Result String PushShiftResult
base64ToPushShiftResult input =
    Base64.toBytes input
        |> Maybe.andThen Flate.inflate
        |> Maybe.andThen (\buffer -> BytesDecode.decode (BytesDecode.string (Bytes.width buffer)) buffer)
        |> Maybe.andThen (\payload -> Result.toMaybe (JsonDecode.decodeString snapShotDecoder payload))
        |> Result.fromMaybe "Bad Data"


serializeSnapShot : PushShiftResult -> Maybe String
serializeSnapShot result =
    UrlBase64.encode pushShiftResultToBase64 result
        |> Result.toMaybe


pushShiftResultToBase64 : PushShiftResult -> Result String String
pushShiftResultToBase64 result =
    Result.fromMaybe "Bad data"
        << Base64.fromBytes
        << Flate.deflateWithOptions (Flate.Dynamic (Flate.WithWindowSize LZ77.maxWindowSize))
        << BytesEncode.encode
        << BytesEncode.string
        << JsonEncode.encode 0
    <|
        snapShotEncoder result


snapShotTimeFormatted : Time.Posix -> String
snapShotTimeFormatted time =
    DateFormat.format
        [ dayOfMonthSuffix
        , text " "
        , monthNameAbbreviated
        , text " "
        , yearNumber
        , text " "
        , hourNumber
        , text ":"
        , minuteFixed
        , text " "
        , amPmLowercase
        , text " UTC"
        ]
        Time.utc
        time
