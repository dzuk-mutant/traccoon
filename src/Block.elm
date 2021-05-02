module Block exposing (Block, fromValues)

import Subtask
import Time


{-| A Block is a span of time worked on a particular
project at a particular stage.
-}
type alias Block =
    { start : Time.Posix
    , end : Time.Posix
    , subtaskID : Maybe Subtask.ID
    }


{-| Creates a block from it's values.
-}
fromValues : Time.Posix -> Time.Posix -> Maybe Subtask.ID -> Block
fromValues start end subtaskID =
    { start = start
    , end = end
    , subtaskID = subtaskID
    }
