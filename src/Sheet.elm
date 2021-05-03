module Sheet exposing
    ( Sheet
    , addProject
    , editProject
    , endCurrentBlock
    , init
    , startCurrentBlock
    , updateTime
    , updateTimeZone
    )

{-| The module that handles the Sheet - the core data structure for Traccoon.
-}

import Array exposing (Array)
import Block
import Project exposing (Project)
import ProjectType exposing (ProjectType)
import Subtask
import Time



---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
----------------------------- TYPES ---------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------

{-| A Sheet is the entire data structure for the app.

The current block is the block being created from the user working on
a project right now (if any). The user can only have one current block
at a time.

-}
type alias Sheet =
    { projectTypes : Array ProjectType
    , projects : Array Project
    , currentBlock : Maybe CurrentBlock

    , time : Time.Posix
    , zone : Time.Zone
    }

{-| A Block that's in progress. It doesn't have an end time
like a normal Block because a CurrentBlock has not ended yet.

The user can only have one CurrentBlock at a time.

A Block may have a Subtask ID depending on the Project's ProjectType.
-}
type alias CurrentBlock =
    { start : Time.Posix
    , projectID : Project.ID
    , subtaskID : Subtask.ID
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
    , currentBlock = Nothing

    -- the time does not start accurately
    -- and must be properly initialised before use.
    , time = Time.millisToPosix 0
    , zone = Time.utc
    }


updateTime : Time.Posix -> Sheet -> Sheet
updateTime newTime sheet =
    { sheet | time = newTime }


updateTimeZone : Time.Zone -> Sheet -> Sheet
updateTimeZone newZone sheet =
    { sheet | zone = newZone }



{-| Adds a Project to the Sheet.
-}
addProject : String -> ProjectType.ID -> Project.MonetaryValue -> Sheet -> Sheet
addProject name projTypeID mValue sheet =
    { sheet
        | projects =
            Array.append (Array.fromList [ Project.fromValues name projTypeID mValue ]) sheet.projects
    }


{-| Edits a project and saves the edit back into the Sheet.

Will just return the Sheet as-is if the given Project ID doesn't exist.

A Project's type cannot be changed.

-}
editProject : String -> Project.MonetaryValue -> Project.ID -> Sheet -> Sheet
editProject newName newMValue projectID sheet =
    case Array.get projectID sheet.projects of
        Nothing ->
            sheet

        Just project ->
            let
                newProject =
                    { project
                        | name = newName
                        , monetaryValue = newMValue
                    }
            in
            { sheet
                | projects = Array.set projectID newProject sheet.projects
            }


{-| Starts a new block.

It creates a new block in the currentBlock, and puts the time this
function was used as the starting time.
-}
startCurrentBlock : Project.ID -> Subtask.ID -> Sheet -> Sheet
startCurrentBlock projectID subtaskID sheet =
    { sheet
        | currentBlock =
            Just
                { start = sheet.time
                , projectID = projectID
                , subtaskID = subtaskID
                }
    }


{-| Ends the current block and attaches it to the record of blocks.

The time this function is called will become that block's end time.

Will return the same sheet with no changes if there is no current
block or if the project cannot be found.
-}
endCurrentBlock : Sheet -> Sheet
endCurrentBlock sheet =
    case sheet.currentBlock of
        Nothing ->
            sheet

        Just currentBlock ->
            let
                projID = currentBlock.projectID
            in
            case Array.get projID sheet.projects of
                Nothing ->
                    sheet

                Just proj ->
                    let
                        newBlock = Block.fromValues currentBlock.start sheet.time currentBlock.subtaskID
                        newProj = Project.addBlock newBlock proj
                    in
                    { sheet
                        | currentBlock = Nothing
                        , projects = Array.set projID newProj sheet.projects
                    }
