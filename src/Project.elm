module Project exposing
    ( ID
    , MonetaryValue(..)
    , Project

    , addBlock
    , deleteBlock
    , edit
    , fromValues

    , toTotalTime
    , toTimeBreakdown
    , toMoneyPerHour
    )

{-| A project is a particular job that has a clear beginning and end.

This module handles the types, creation, manipulation and conversion
of these projects.
-}

import Array exposing (Array)
import Block exposing (Block)
import Currency
import Dict exposing (Dict)
import ProjectType
import Subtask
import Time



---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
------------------------------- TYPES -------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


type alias ID = Int


{-| Blocks are a Dict so they can be deleted and edited.

The key of blocks in a project is the project's start time as an Int.

( `key == Time.posixToMillis block.start` )
-}
type alias Blocks = Dict Int Block


{-| A Project is a particular work job.

Blocks are the periods of time worked on this particular project.
They are listed chronologically from the earliest to the latest.
There should never be overlapping blocks.

-}
type alias Project =
    { name : String
    , projTypeID : ProjectType.ID
    , monetaryValue : MonetaryValue
    , blocks : Blocks
    }


{-| The monetary value of a project (if any.)
-}
type MonetaryValue
    = None
    | FlatFee Currency.Value
    | Hourly Currency.Value


---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
--------------------- CREATION + MANIPULATION -----------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Creates a Project from it's base values.
-}
fromValues : String -> ProjectType.ID -> MonetaryValue -> Project
fromValues name projTypeID value =
    { name = name
    , projTypeID = projTypeID
    , monetaryValue = value
    , blocks = Dict.empty -- Projects start with no blocks
    }


{-| Adds a block to a Project.
-}
addBlock : Block -> Project -> Project
addBlock newBlock proj =
    { proj | blocks = Dict.update (Time.posixToMillis newBlock.start) (\_ -> Just newBlock) proj.blocks }


{-| Deletes a block from a Project.

If the ID doesn't exist, then the returned Project is the same.
-}
deleteBlock : Int -> Project -> Project
deleteBlock blockID proj =
    { proj | blocks = Dict.remove blockID proj.blocks }



{-| Takes an existing Project returns the same Project with a new
name and monetary value and set of blocks that's been given.

(For technical reasons, a Project's ProjectType cannot be changed
after it's been created.)
-}
edit : String -> MonetaryValue -> Project -> Project
edit name monetaryValue proj =
    { proj
        | name = name
        , monetaryValue = monetaryValue
    }

---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
-------------------- EXTRACTIONS/CONVERSIONS ------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------



{-| A function that returns the total amount of time
spent on a project as an Int in milliseconds.
-}
toTotalTime : Project -> Int
toTotalTime proj =
    proj.blocks
        |> Dict.values
        |> List.map Block.toTimeLength
        |> List.foldl (+) 0


{-| Returns a Dict, the key is the Subtask ID and the value is the
total time length of all the blocks that have that Subtask ID.

This is so a breakdown of how much the user has spent on particular
subtasks can be presented.
-}
toTimeBreakdown : Project -> Dict Subtask.ID Int
toTimeBreakdown proj =
    let
        maybeAdd = (\block v -> 
            case v of
               Nothing -> Just <| Block.toTimeLength block
               Just vv -> Just <| vv + Block.toTimeLength block
            )
        accumulate = (\block totals -> Dict.update block.subtaskID (maybeAdd block) totals)
    in
        proj.blocks
        |> Dict.values
        |> List.foldl accumulate Dict.empty



{-| Takes a Project and returns the amount of money earned
per hour on that Project.

If the Project didn't have a monetary value to begin with,
then this function returns Nothing.
-}
toMoneyPerHour : Project -> Maybe Currency.Value
toMoneyPerHour proj =
    case proj.monetaryValue of
        None ->
            Nothing

        FlatFee val ->
            proj
                |> toTotalTime
                |> (\v -> toFloat v / 3600000) -- convert to hours
                |> (\v -> val.amount / v)
                |> (\v -> Just { val | amount = v }) -- return the monthly val in the same currency.

        Hourly val ->
            Just val
