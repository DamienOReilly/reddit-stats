module ChartUtils exposing (chartConstructor, getAggregatedAxisData)

import Chartjs.Chart as Chart
import Chartjs.Common as ChartCommon
import Chartjs.Data as ChartData
import Chartjs.DataSets.Bar as BarData
import Chartjs.DataSets.DoughnutAndPie as DoughnutAndPieData
import Chartjs.DataSets.Line as LineData
import Chartjs.Options as ChartOptions
import Chartjs.Options.Legend as ChartLegend
import Chartjs.Options.Title as ChartTitle
import Codecs exposing (AxisData(..), AxisDataType(..), IntermPostCountPerX, PushShiftData(..), PushShiftPostCount, PushShiftPostCountSubReddit)
import Color exposing (Color)
import Constants
import DateFormat
import Dict
import Dict.Extra
import List.Extra
import Time exposing (Month(..), millisToPosix, toYear, utc)
import Utils exposing (descending, sortByWith)


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
