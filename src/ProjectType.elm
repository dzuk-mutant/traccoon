module ProjectType exposing
    ( ID
    , ProjectType
    , Breakdown(..)
    , Err(..)

    , fromValues
    , editName
    , addSubtask
    , deleteSubtask

    , getSubtaskColor
    , hasSubtask
    )

import Dict exposing (Dict)
import Color exposing (Color)
import Subtask exposing (Subtask)
import Helper



---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
----------------------------- TYPES ---------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


type alias ID =
    Int


{-| A Project type notes what kind of project it is.
-}
type alias ProjectType =
    { name : String
    , breakdown : Breakdown
    }


{-| A Project type can be:

  - Monolithic (it cannot be broken down into subtasks)
  - HasSubtasks (it can be broken down into subtasks)

A Project with a Monolithic ProjectType will still have
Subtask IDs (because of specific technical restrictions of Elm),
it's just that all the IDs will be zero.

-}
type Breakdown
    = Monolithic Color
    | Subtasked (Dict Int Subtask)


{-| Errors that can happen in particular ProjectType operations.

- ProjTypeHasNoSubtasks: A subtask was attempted to be edited
or accessed when the ProjectType is Monolithic.
- SubtaskNotFound: A subtask was attempted to be edited or
accessed when it doesn't exist.
-}
type Err
    = ProjTypeHasNoSubtasks
    | SubtaskNotFound


---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
--------------------- CREATION + MANIPULATION -----------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Creates a ProjectType from it's base values.
-}
fromValues : String -> Breakdown -> ProjectType
fromValues name breakdown =
    { name = name
    , breakdown = breakdown
    }


{-| Edits a ProjectType's name.

The breakdown of a ProjectType cannot be changed.

-}
editName : String -> ProjectType -> ProjectType
editName newName projType =
    { projType | name = newName }


{-| Adds a subtask to a ProjectType. If a ProjectType is Monolithic,
it will return an error.
-}
addSubtask : Subtask -> ProjectType -> Result Err ProjectType
addSubtask newSubtask projType =
    case projType.breakdown of
        Monolithic _ -> Err ProjTypeHasNoSubtasks
        Subtasked subtasks ->
            let
                newKey = Helper.getNewIncrementedDictKey subtasks
            in
            Ok { projType | breakdown = Subtasked <| Dict.insert newKey newSubtask subtasks  }


{-| Deletes a subtask that's at a given ID. Will return
an error if the ProjectType is Monolithic or the ID doesn't
exist.

**This function should never be used in isolation - Blocks with this
subtaskID must be replaced with an alternative.**
-}
deleteSubtask : Subtask.ID -> ProjectType -> Result Err ProjectType
deleteSubtask subtaskID projType =
    if not <| hasSubtask subtaskID projType then
        Err SubtaskNotFound
    else
        case projType.breakdown of
            Monolithic _ -> Err ProjTypeHasNoSubtasks
            Subtasked subtasks ->
                Ok { projType | breakdown = Subtasked <| Dict.remove subtaskID subtasks }


---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
----------------------------- QUERIES -------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Tries to get a colour from a Subtask ID. If the ProjectType is
Monolithic, then it returns the monolithic color.
-}
getSubtaskColor : Subtask.ID -> ProjectType -> Maybe Color
getSubtaskColor subtaskID projType =
    case projType.breakdown of
        Monolithic m -> Just m
        Subtasked subtasks ->
            Maybe.map .color (Dict.get subtaskID subtasks)


{-| Checks if a subtask exists in a ProjectType.
-}
hasSubtask : Subtask.ID -> ProjectType -> Bool
hasSubtask subtaskID projType =
    case projType.breakdown of
        Monolithic _ -> subtaskID == 0
        Subtasked subtasks ->
            Dict.member subtaskID subtasks
            