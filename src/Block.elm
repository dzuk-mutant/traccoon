module Block exposing (Block, fromValues, toTimeLength)

import Subtask
import Time


{-| A Block is a span of time worked on a particular
project at a particular stage.
-}
type alias Block =
    { start : Time.Posix
    , end : Time.Posix
    , subtaskID : Subtask.ID
    }


{-| Creates a block from it's values.
-}
fromValues : Time.Posix -> Time.Posix -> Subtask.ID -> Block
fromValues start end subtaskID =
    { start = start
    , end = end
    , subtaskID = subtaskID
    }


{-| Shorthand for getting a time length (as an
Int in milliseconds) from a block's start and end.
-}
toTimeLength : Block -> Int
toTimeLength block =
    Time.posixToMillis block.end - Time.posixToMillis block.start
