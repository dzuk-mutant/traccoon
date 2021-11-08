module ProjectType exposing
    ( Err(..)
    , ID
    , ProjectType
    , addSubtask
    , deleteSubtask
    , editName
    , fromValues
    , getSubtask
    , hasSubtask
    )

import Dict exposing (Dict)
import Helper
import Subtask exposing (Subtask)



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
    , subtasks : Dict Int Subtask
    }


{-| Errors that can happen in particular ProjectType operations.

  - ProjTypeHasNoSubtasks: A subtask was attempted to be edited
    or accessed when the ProjectType is Monolithic.
  - SubtaskNotFound: A subtask was attempted to be edited or
    accessed when it doesn't exist.

-}
type Err
    = SubtaskNotFound



---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
--------------------- CREATION + MANIPULATION -----------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Creates a ProjectType from it's base values.
-}
fromValues : String -> Dict Int Subtask -> ProjectType
fromValues name subtasks =
    { name = name
    , subtasks = subtasks
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
    let
        newKey =
            Helper.getNewIncrementedDictKey projType.subtasks
    in
    Ok { projType | subtasks = Dict.insert newKey newSubtask projType.subtasks }


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
        Ok { projType | subtasks = Dict.remove subtaskID projType.subtasks }



---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
----------------------------- QUERIES -------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Tries to get a Subtask from a Subtask ID.
-}
getSubtask : Subtask.ID -> ProjectType -> Result Err Subtask
getSubtask subtaskID projType =
    case Dict.get subtaskID projType.subtasks of
        Nothing ->
            Err SubtaskNotFound

        Just result ->
            Ok result


{-| Checks if a subtask exists in a ProjectType.
-}
hasSubtask : Subtask.ID -> ProjectType -> Bool
hasSubtask subtaskID projType =
    Dict.member subtaskID projType.subtasks
