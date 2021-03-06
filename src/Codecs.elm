module Codecs exposing (AxisData(..), AxisDataType(..), IntermCountPerX, PushShiftCount, PushShiftCountSubReddit, PushShiftData(..), PushShiftResult(..), SnapshotResult(..), User(..), countDecoder, countSubRedditDecoder, deserializeSnapShot, pushShiftAggDecoder, serializeSnapShot, snapShotTimeFormatted)

import Base64
import Bytes
import Bytes.Decode as BytesDecode
import Bytes.Encode as BytesEncode
import Constants
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
    = RedditCommentCountPerSubReddit (List PushShiftCountSubReddit)
    | RedditSubmissionCountPerSubReddit (List PushShiftCountSubReddit)
    | RedditCommentCount (List PushShiftCount)
    | RedditSubmissionCount (List PushShiftCount)


type PushShiftResult
    = PushShiftResult User Time.Posix (List PushShiftData)


type SnapshotResult
    = SnapshotResult User Time.Posix (List AxisDataType)


type AxisData
    = AxisData (List String) (List Float)


type AxisDataType
    = CommentCountPerYear AxisData
    | SubmissionCountPerYear AxisData
    | CommentCountPerSubReddit AxisData
    | SubmissionCountPerSubReddit AxisData
    | CommentCountPerMonth AxisData
    | SubmissionCountPerMonth AxisData


type alias PushShiftCount =
    { date : Int
    , count : Int
    }


type alias IntermCountPerX =
    { date : String
    , count : Int
    }


type alias PushShiftCountSubReddit =
    { subreddit : String
    , count : Int
    }


countDecoder : Decoder PushShiftCount
countDecoder =
    JsonDecode.map2 PushShiftCount
        (JsonDecode.field "key" JsonDecode.int)
        (JsonDecode.field "doc_count" JsonDecode.int)


countSubRedditDecoder : Decoder PushShiftCountSubReddit
countSubRedditDecoder =
    JsonDecode.map2 PushShiftCountSubReddit
        (JsonDecode.field "key" JsonDecode.string)
        (JsonDecode.field "doc_count" JsonDecode.int)


pushShiftAggDecoder : Decoder a -> String -> Decoder (List a)
pushShiftAggDecoder decoder aggregator =
    JsonDecode.map identity (JsonDecode.field "aggs" (JsonDecode.field aggregator (JsonDecode.list decoder)))


axisDataEncoder : AxisData -> JsonEncode.Value
axisDataEncoder (AxisData x y) =
    JsonEncode.object
        [ ( "x", JsonEncode.list JsonEncode.string x )
        , ( "y", JsonEncode.list JsonEncode.float y )
        ]


axisDataDecoder : Decoder AxisData
axisDataDecoder =
    JsonDecode.map2 AxisData
        (JsonDecode.field "x" <| JsonDecode.list JsonDecode.string)
        (JsonDecode.field "y" <| JsonDecode.list JsonDecode.float)


{-| Would like to now if its possible to do this without the intermediate type aliases
-}
snapshotDataEncoder : List AxisDataType -> JsonEncode.Value
snapshotDataEncoder axisDataList =
    axisDataList
        |> List.map
            (\data ->
                case data of
                    CommentCountPerYear count ->
                        ( "py", axisDataEncoder count )

                    CommentCountPerMonth count ->
                        ( "pm", axisDataEncoder count )

                    SubmissionCountPerYear count ->
                        ( "sy", axisDataEncoder count )

                    SubmissionCountPerMonth count ->
                        ( "sm", axisDataEncoder count )

                    CommentCountPerSubReddit count ->
                        ( "ps", axisDataEncoder count )

                    SubmissionCountPerSubReddit count ->
                        ( "ss", axisDataEncoder count )
            )
        |> JsonEncode.object


type alias IntermediateSnapshotData =
    { commentCountPerSubReddit : AxisData
    , submissionCountPerSubReddit : AxisData
    , commentCountPerYear : AxisData
    , commentCountPerMonth : AxisData
    , submissionCountPerYear : AxisData
    , submissionCountPerMonth : AxisData
    }


snapshotDataDecoder : Decoder (List AxisDataType)
snapshotDataDecoder =
    JsonDecode.map6 IntermediateSnapshotData
        (JsonDecode.field "ps" axisDataDecoder)
        (JsonDecode.field "ss" axisDataDecoder)
        (JsonDecode.field "py" axisDataDecoder)
        (JsonDecode.field "pm" axisDataDecoder)
        (JsonDecode.field "sy" axisDataDecoder)
        (JsonDecode.field "sm" axisDataDecoder)
        |> JsonDecode.map
            (\x ->
                [ CommentCountPerSubReddit x.commentCountPerSubReddit
                , SubmissionCountPerSubReddit x.submissionCountPerSubReddit
                , CommentCountPerYear x.commentCountPerYear
                , CommentCountPerMonth x.commentCountPerMonth
                , SubmissionCountPerYear x.submissionCountPerYear
                , SubmissionCountPerMonth x.submissionCountPerMonth
                ]
            )


snapShotEncoder : SnapshotResult -> JsonEncode.Value
snapShotEncoder (SnapshotResult (User user) time data) =
    JsonEncode.object
        [ ( "v", JsonEncode.int Constants.version )
        , ( "u", JsonEncode.string user )
        , ( "t", JsonEncode.int <| Time.posixToMillis time )
        , ( "d", snapshotDataEncoder data )
        ]


type alias IntermediateSnapshotResult =
    { version : Int
    , user : String
    , snapShotTime : Int
    , data : List AxisDataType
    }


snapShotDecoder : Decoder SnapshotResult
snapShotDecoder =
    JsonDecode.map4 IntermediateSnapshotResult
        (JsonDecode.field "v" JsonDecode.int)
        (JsonDecode.field "u" JsonDecode.string)
        (JsonDecode.field "t" JsonDecode.int)
        (JsonDecode.field "d" snapshotDataDecoder)
        |> JsonDecode.map
            (\x -> SnapshotResult (User x.user) (Time.millisToPosix x.snapShotTime) x.data)


deserializeSnapShot : String -> Maybe SnapshotResult
deserializeSnapShot input =
    UrlBase64.decode base64ToPushShiftResult input
        |> Result.toMaybe


base64ToPushShiftResult : String -> Result String SnapshotResult
base64ToPushShiftResult input =
    Base64.toBytes input
        |> Maybe.andThen Flate.inflate
        |> Maybe.andThen (\buffer -> BytesDecode.decode (BytesDecode.string (Bytes.width buffer)) buffer)
        |> Maybe.andThen (\payload -> Result.toMaybe (JsonDecode.decodeString snapShotDecoder payload))
        |> Result.fromMaybe "Bad Data"


serializeSnapShot : SnapshotResult -> Maybe String
serializeSnapShot result =
    UrlBase64.encode snapshotToBase64 result
        |> Result.toMaybe


snapshotToBase64 : SnapshotResult -> Result String String
snapshotToBase64 result =
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
    format
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
