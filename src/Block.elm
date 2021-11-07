module Block exposing (Block, fromValues, toTimeLength, partlyOverlaps, compare)

import Subtask
import Time
import Timeframe exposing (Timeframe)

{-| A Block is a span of time worked on a particular
project at a particular stage.
-}
type alias Block =
    { timeframe : Timeframe
    , subtaskID : Subtask.ID
    }


{-| Creates a block from it's values.
-}
fromValues : Time.Posix -> Time.Posix -> Subtask.ID -> Block
fromValues start end subtaskID =
    { timeframe = Timeframe.fromPosix start end
    , subtaskID = subtaskID
    }


{-| Shorthand for getting a time length (as an
Int in milliseconds) from a block's start and end.
-}
toTimeLength : Block -> Int
toTimeLength block =
    Timeframe.toLength block.timeframe


{-| Checks if a block partly overlaps the given timeframe.
-}
partlyOverlaps : Timeframe -> Block -> Bool
partlyOverlaps timeframe block =
    Timeframe.partlyOverlaps timeframe block.timeframe


{-| Checks which block starts first chronologically.
-}
compare : Block -> Block -> Order
compare block1 block2 =
    Timeframe.compare block1.timeframe block2.timeframe
