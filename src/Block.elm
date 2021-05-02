module Block exposing (Block, fromTimes)

import Time


{-| A Block is a span of time worked on a particular
project at a particular stage.
-}
type alias Block =
    { start : Time.Posix
    , end : Time.Posix

    -- add a subtask!
    }


{-| Creates a block from a start time and an end time.
-}
fromTimes : Time.Posix -> Time.Posix -> Block
fromTimes start end =
    { start = start
    , end = end
    }
