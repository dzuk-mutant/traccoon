module Timeframe exposing (Timeframe, fromPosix, fromMillis, encompasses, partlyOverlaps, compare, toLength)

{-| A module for conveniently handling and comparing spans of time.

# Types
@docs Timeframe, fromPosix, fromMillis

# Comparison
@docs encompasses, partlyOverlaps, compare

# Conversion
@docs toLength
-}

import Time


{-| A data type representing a particular span of time. Has a start and an end value.
-}
type alias Timeframe =
    { start : Time.Posix
    , end : Time.Posix
    }


{-| Creates a Timeframe from two Time.Posix values.
First argument is the start of the timeframe, second argument is the end.
-}
fromPosix : Time.Posix -> Time.Posix -> Timeframe
fromPosix start end =
    { start = start
    , end = end
    }


{-| Creates a Timeframe from two Ints representing UNIX time.
First argument is the start of the timeframe, second argument is the end.
-}
fromMillis : Int -> Int -> Timeframe
fromMillis start end =
    { start = Time.millisToPosix start
    , end = Time.millisToPosix  end
    }


{-| Checks whether a Timeframe encompasses a given Time.Posix.
-}
encompasses : Timeframe -> Time.Posix -> Bool
encompasses timeframe timePosix =
    let
        start = Time.posixToMillis timeframe.start
        end = Time.posixToMillis timeframe.end
        time = Time.posixToMillis timePosix
    in
    time >= start && time <= end


{-| Checks whether the second Timeframe at least partially overlaps the first one.
-}
partlyOverlaps : Timeframe -> Timeframe -> Bool
partlyOverlaps timeframe1 timeframe2 =
    encompasses timeframe1 timeframe2.start
    || encompasses timeframe1 timeframe2.end


{-| Compares two timeframes and returns an Order based on
which one started first.

Equivalent to `Basics.compare` in functionality.
-}
compare : Timeframe -> Timeframe -> Order
compare timeframe1 timeframe2 =
    compare (Time.posixToMillis timeframe1.start) (Time.posixToMillis timeframe2.start)


{-| Returns the length between the start and end of the
timeframe as an Int representing milliseconds.
-}
toLength : Timeframe -> Int
toLength timeframe =
    Time.posixToMillis timeframe.end - Time.posixToMillis timeframe.start
