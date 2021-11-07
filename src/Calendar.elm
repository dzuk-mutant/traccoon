module Calendar exposing (millisToHours, weekSpanFromTime)

import Time exposing (Weekday(..))
import Timeframe exposing (Timeframe)
import Html exposing (time)



{-| A second in milliseconds.
-}
second : Int
second = 1000

{-| A minute in milliseconds.
-}
minute : Int
minute = second * 60

{-| An hour in milliseconds.
-}
hour : Int
hour = minute * 60

{-| A day in milliseconds.
-}
day : Int
day = hour * 24

{-| A week in milliseconds.
-}
week : Int
week = day * 7


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
        nowWeekdayOffset = weekdayToStartOffset <| Time.toWeekday zone time
        nowHour = Time.toHour zone time
        nowMinute = Time.toMinute zone time
        nowSecond = Time.toSecond zone time
        nowMillis = Time.toMillis zone time

        -- get us to Monday at 00:00.
        startMillis = 
            time
            |> Time.posixToMillis
            |> (-) (nowWeekdayOffset * day)
            |> (-) (nowHour * hour)
            |> (-) (nowMinute * minute)
            |> (-) (nowSecond * second)
            |> (-) (nowMillis)
        
        start = startMillis

        -- get us to Sunday at 23:59:59:999.
        end = startMillis + week - 1

    in
        Timeframe.fromMillis start end


{-| Internal function that determines how far you have to count back
to get to Monday from the current weekday.
-}
weekdayToStartOffset : Time.Weekday -> Int
weekdayToStartOffset weekday =
    case weekday of
       Mon -> 0
       Tue -> 1
       Wed -> 2
       Thu -> 3
       Fri -> 4
       Sat -> 5
       Sun -> 6
