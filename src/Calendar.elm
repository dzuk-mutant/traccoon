module Calendar exposing
    ( day
    , hour
    , millisToHours
    , minute
    , second
    , week
    , weekSpanFromTime
    )

import Html exposing (time)
import Time exposing (Weekday(..))
import Timeframe exposing (Timeframe)


{-| A second in milliseconds.
-}
second : Int
second =
    1000


{-| A minute in milliseconds.
-}
minute : Int
minute =
    second * 60


{-| An hour in milliseconds.
-}
hour : Int
hour =
    minute * 60


{-| A day in milliseconds.
-}
day : Int
day =
    hour * 24


{-| A week in milliseconds.
-}
week : Int
week =
    day * 7


{-| Converts an int representing milliseconds
into a float representing hours.
-}
millisToHours : Int -> Float
millisToHours millis =
    toFloat millis / toFloat hour


{-| Returns a Timeframe containing:

  - Start time at Monday at 00:00:00:000
  - End time at Sunday at 23:59:59:999

That's in the same week as the given time and
in the same time zone as the given timezone.

-}
weekSpanFromTime : Time.Zone -> Time.Posix -> Timeframe
weekSpanFromTime zone time =
    let
        nowWeekdayOffset : Int
        nowWeekdayOffset = weekdayToStartOffset zone time

        nowHour : Int
        nowHour = Time.toHour zone time

        nowMinute : Int
        nowMinute = Time.toMinute zone time

        nowSecond : Int
        nowSecond = Time.toSecond zone time

        nowMillis : Int
        nowMillis = Time.toMillis zone time

        -- get us to Monday at 00:00.
        start : Int
        start =
            time
                |> Time.posixToMillis
                |> (-) (nowWeekdayOffset * day)
                |> (-) (nowHour * hour)
                |> (-) (nowMinute * minute)
                |> (-) (nowSecond * second)
                |> (-) nowMillis


        -- get us to Sunday at 23:59:59:999.
        end : Int
        end = start + week - 1
    in
    Timeframe.fromMillis start end






{-| Internal function that determines how far you have to count back
to get to Monday from the current weekday.
-}
weekdayToStartOffset : Time.Zone -> Time.Posix -> Int
weekdayToStartOffset zone time =
    let
        weekday =
            Time.toWeekday zone time
    in
    case weekday of
        Mon ->
            0

        Tue ->
            1

        Wed ->
            2

        Thu ->
            3

        Fri ->
            4

        Sat ->
            5

        Sun ->
            6
