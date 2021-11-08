module View.Timeline exposing (weekView)


{-| For creating timeline views with blocks in a linear graph.

@docs weekView
-}

import Array exposing (Array)
import Block
import Calendar
import Color exposing (Color)
import Css exposing (borderRadius, height, marginLeft, marginRight, pct, width)
import Externalized as Ext
import Helper
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Rpx exposing (blc, rpx)
import Sheet exposing (Sheet, Time(..), TimeZone(..))
import Subtask exposing (Subtask)
import Time
import Timeframe exposing (Timeframe)








type alias Day =
    { blocks : List Ext.Block
    , timeframe : Timeframe
    }

{-| Creates a day from a timeframe and a list of Externalized.Blocks
to be filtered by that timeframe.
-}
dayFromUnfilteredValues : Timeframe -> List Ext.Block -> Day
dayFromUnfilteredValues timeframe blocks =
    { blocks = Ext.filterBlocks blocks timeframe
    , timeframe = timeframe
    }

type alias Timeline = Array Day


{-| Gets a next consecutive day either ahead or behind the
timeframe given based on the input number. eg. 1 is the day ahead, and -1 is the day before.

This is providing that the given timeframe is an exact day.
If the timeframe isn't an exact day, you will get a weird result.

-}
getSubsequentDay : Timeframe -> List Ext.Block -> Int -> Day
getSubsequentDay dayFrame blocks offset =
    case compare offset 0 of
        EQ -> dayFromUnfilteredValues dayFrame blocks

        LT ->
            dayFromUnfilteredValues
                ( Timeframe.fromMillisOffset
                    (Time.posixToMillis dayFrame.start - (abs offset * Calendar.day))
                    (Calendar.day - 1)
                )
                blocks

        GT ->
            dayFromUnfilteredValues
                ( Timeframe.fromMillisOffset
                    (Time.posixToMillis dayFrame.end + ((offset - 1) * Calendar.day) + 1)
                    (Calendar.day - 1)
                )
                blocks


createWeek : Time.Zone -> Time.Posix -> Sheet -> Timeline
createWeek zone time sheet =
    let
        week : Timeframe
        week =
            Calendar.weekSpanFromTime zone time

        blocks : List Ext.Block
        blocks =
            sheet
                |> Sheet.toProjectsFilteredByTimeframe week
                |> Ext.blocksFromProjectDict
                |> Ext.sortBlocks

        day1 : Timeframe
        day1 =
            Timeframe.fromMillisOffset
                (Time.posixToMillis week.start)
                (Calendar.day - 1)
    in
        Array.initialize 7 (getSubsequentDay day1 blocks)











{-| Creates a timeline in the calendrical week
that the given time is in.
-}
weekView : Sheet -> Time.Posix -> Html msg
weekView sheet time =
    let
        noView =
            Html.div [] []
    in
    case sheet.time of
        NoTime ->
            noView

        HasTime currentTime ->
            case sheet.zone of
                NoTZ ->
                    noView

                HasTZ zone ->
                    weeklyView zone currentTime time sheet


weeklyView : Time.Zone -> Time.Posix -> Time.Posix -> Sheet -> Html msg
weeklyView zone currentTime time sheet =
    let
        week : Timeline
        week = createWeek zone time sheet
    in
    Html.div
        []
        [ graph currentTime sheet week
        ]


{-| The graph component of the timeline, without the legends.
-}
graph : Time.Posix -> Sheet -> Timeline -> Html msg
graph currentTime sheet tl =
    Html.div
        [ css []
        ]
        ( tl
            |> Array.toList
            |> List.map (dayTrack currentTime sheet)
        )


{-| The day track component of the timeline graph -
 a single vertical column showing blocks for a single day.
-}
dayTrack : Time.Posix -> Sheet -> Day -> Html msg
dayTrack currentTime sheet day =
    Html.div
        [ css
            [ width (blc 5)
            , marginLeft (blc 0.5)
            , marginRight (blc 0.5)
            ]
        ]
        [ Html.div
            []
            ( List.map (blockView currentTime sheet) day.blocks )
        , ( currentTimeDot currentTime sheet day )
        ]


{-| Renders a single block in a day track.
-}
blockView : Time.Posix -> Sheet -> Ext.Block -> Html msg
blockView currentTime sheet block =
    let
        subtaskRes : Result Sheet.Err Subtask
        subtaskRes =
            Sheet.getSubtask block.projTypeID block.block.subtaskID sheet

        subtaskColor : Color
        subtaskColor =
            case subtaskRes of
                Err _ ->
                    Color.rgb255 15 15 15

                Ok subtask ->
                    subtask.color
    in
    Html.div
        [ css
            [ height <| pct ((toFloat <| Block.toTimeLength block.block) / toFloat Calendar.day)
            , borderRadius (rpx 8)
            , Helper.colorFromType subtaskColor
            ]
        ]
        []

{-| Generates the dot representing the current day, if the 
current time is within the current day.
-}
currentTimeDot : Time.Posix -> Sheet -> Day -> Html msg
currentTimeDot currentTime sheet day =
    Html.div [] []
