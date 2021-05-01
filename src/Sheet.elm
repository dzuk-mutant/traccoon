module Sheet exposing ( Block
                      , Sheet

                      , init
                      
                      , addProject
                      , editProject

                      , startCurrentBlock
                      , endCurrentBlock
                      )


{-| The module that handles the Sheet - the core data structure for Traccoon.
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Project exposing (Project)
import ProjectType exposing (ProjectType)
import Subtask
import Time


{-| A Sheet is the entire data structure for the app.

Blocks are listed chronologically from the earliest to the latest, 
their Dict key is the start time as an Int in POSIX milliseconds. There
should never be overlapping blocks.

The current block is the block being created from the user working on
a project right now (if any). The user can only have one current block
at a time.

-}
type alias Sheet =
    { projectTypes : Array ProjectType
    , projects : Array Project
    , blocks : Dict Int Block
    , currentBlock : Maybe CurrentBlock
    }


{-| A Block is a span of time worked on a particular
project at a particular stage.
-}
type alias Block =
    { start : Time.Posix
    , end : Time.Posix
    , project : Project.ID
    , stage : Subtask.ID
    }


{-| A Block that's in progress. It doesn't have an end time
like a normal Block because a CurrentBlock has not ended yet.

The user can only have one CurrentBlock at a time.
-}
type alias CurrentBlock =
    { start : Time.Posix
    , project : Project.ID
    , stage : Subtask.ID
    }



---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------- CREATE AND EDIT SHEETS -----------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------

{-| Creates a blank Sheet.
-}
init : Sheet
init =
    { projectTypes = Array.empty
    , projects = Array.empty
    , blocks = Dict.empty
    , currentBlock = Nothing
    }


{-| Adds a Project to the Sheet.
-}
addProject : String -> ProjectType.ID -> Project.MonetaryValue -> Sheet -> Sheet
addProject name projTypeID mValue sheet =
    { sheet | projects = 
        Array.append ( Array.fromList [ Project.create name projTypeID mValue ] ) sheet.projects
    }

{-| Edits a project and saves the edit back into the Sheet.

Will just return the Sheet as-is if the given Project ID doesn't exist.

A Project's type cannot be changed.
-}
editProject : String -> Project.MonetaryValue -> Project.ID -> Sheet -> Sheet
editProject newName newMValue projectID sheet =
    case Array.get projectID sheet.projects of
        Nothing -> sheet
        Just project ->
            let
                newProject =
                    { project | name = newName
                              , monetaryValue = newMValue
                    }
            in
                { sheet | projects = Array.set projectID newProject sheet.projects
                }


{-| Starts a new block.
-}
startCurrentBlock : Time.Posix -> Project.ID -> Subtask.ID -> Sheet -> Sheet
startCurrentBlock startTime project stage sheet =
    { sheet
        | currentBlock =
            Just
                { start = startTime
                , project = project
                , stage = stage
                }
    }


{-| Ends the current block and attaches it to the record of blocks.

Will return the same sheet with no changes if there is no current block.
-}
endCurrentBlock : Time.Posix -> Sheet -> Sheet
endCurrentBlock endTime sheet =
    case sheet.currentBlock of
        Nothing ->
            sheet
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
