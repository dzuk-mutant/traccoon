module TimeSheet exposing ( Block
                          , Sheet

                          , init
                          , startNewBlock
                          , endNewBlock
                          )

import Color exposing (Color)
import Dict exposing (Dict)
import Time


type alias ProjectID = String
type alias StageID = String
type alias ProjectTypeID = String

{-| A Sheet is the entire data structure for the app.

  - Blocks are listed chronologically from the earliest to the latest, 
    their Dict key is the start time as an Int in POSIX milliseconds.
  - The current block is the current block being counted down from (if any).

-}
type alias Sheet =
    { projectTypes : Dict ProjectTypeID ProjectType
    , projects : Dict ProjectID Project
    , blocks : Dict Int Block
    , currentBlock : Maybe CurrentBlock
    }


{-| A Project type notes what kind of project it is.
-}
type alias ProjectType =
    { name : String
    , stages : Dict StageID Stage
    }


{-| A project may have multiple stages. This allows the
user to keep track of not just the jobs they are doing
but sub-tasks within those jobs.

The stage is defined by the project type.

-}
type alias Stage =
    { name : String
    , color : Color
    }


{-| A Project is a particular work job.
-}
type alias Project =
    { name : String
    , projectType : ProjectType
    }


{-| A Block is a span of time worked on a particular
project at a particular stage.
-}
type alias Block =
    { start : Time.Posix
    , end : Time.Posix
    , project : ProjectID
    , stage : Stage
    }


{-| A Block that's in progress.
-}
type alias CurrentBlock =
    { start : Time.Posix
    , project : ProjectID
    , stage : Stage
    }



---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------- CREATE AND EDIT SHEETS -----------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


init : Sheet
init =
    { projectTypes = Dict.empty
    , projects = Dict.empty
    , blocks = Dict.empty
    , currentBlock = Nothing
    }


{-| Starts a new block.
-}
startNewBlock : Time.Posix -> ProjectID -> Stage -> Sheet -> Sheet
startNewBlock startTime project stage sheet =
    { sheet
        | currentBlock =
            Just
                { start = startTime
                , project = project
                , stage = stage
                }
    }


{-| Ends the current block and attaches it to the record of blocks.
-}
endNewBlock : Time.Posix -> Sheet -> Sheet
endNewBlock endTime sheet =
    case sheet.currentBlock of
        Nothing ->
            sheet

        -- just return the sheet as-is
        Just currentBlock ->
            let
                newBlock =
                    { start = currentBlock.start
                    , end = endTime
                    , project = currentBlock.project
                    , stage = currentBlock.stage
                    }
            in
            { sheet
                | currentBlock = Nothing
                , blocks = Dict.insert (Time.posixToMillis newBlock.start) newBlock sheet.blocks
            }
