module Sheet exposing
    ( Sheet
    , Err

    , init

    , updateTime
    , updateTimeZone
    
    , addProjectType
    , editProjectType
    , addProject
    , editProject
    , deleteProject

    , startCurrentBlock
    , endCurrentBlock
    
    , toProjectsFilteredBySubtask
    , toProjectsFilteredByType
    
    , replaceSubtaskIDs
    )

{-| The module that handles the Sheet - the core data structure for Traccoon.

# Types
@docs Sheet, Err

# Init and updating the time
@docs init, updateTime, updateTimeZone

# Creating and editing data points
## ProjectTypes
@docs addProjectType, editProjectType

## Projects
@docs addProject, editProject, deleteProject

# Creating new blocks
@docs startCurrentBlock, endCurrentBlock

# Filtering data
@docs toProjectsFilteredByType, toProjectsFilteredBySubtask

# Mass edits
@docs replaceSubtaskIDs
-}

import Block
import Dict exposing (Dict)
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
    { projTypes : Dict Int ProjectType
    , projects : Dict Int Project
    , currentBlock : Maybe CurrentBlock
    , time : Time
    , zone : TimeZone
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


{-| A wrapper for the time to make sure it's been initialised
before it can be used.
-}
type Time
    = HasTime Time.Posix
    | NoTime


{-| A wrapper for the time zone to make sure it's been initialised
before it can be used.
-}
type TimeZone
    = HasTZ Time.Zone
    | NoTZ


{-| The kinds of errors that can be generated when a Sheet function
encounters a problem:

- TimeNotInitialised - The time has not been initialised yet and thus cannot be used.
- NoCurrentBlock - Tried to do something with the currentBlock even though there isn't one there.
- ProjNotFound - Tried to access, use or edit a Project that's not there.
- ProjTypeNotFound - Tried to access, use or edit a ProjectType that's not there.
- SubtaskIDNotFound - Tried to access, use or edit a Subtask ID that's not there.
- BlockTimeOverlap - A block has been stopped at the same time as it's start, which cannot be allowed to happen.

-}
type Err
    = TimeNotInitialised
    | NoCurrentBlock
    | ProjNotFound
    | ProjTypeNotFound
    | SubtaskIDNotFound
    | BlockTimeOverlap


---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
------------------------------- INIT --------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Creates a blank Sheet.
-}
init : Sheet
init =
    { projTypes = Dict.empty
    , projects = Dict.empty
    , currentBlock = Nothing

    -- time and time zone must be initialised before they can be used.
    , time = NoTime
    , zone = NoTZ
    }


---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
--------------------------- UPDATE TIME -----------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


updateTime : Time.Posix -> Sheet -> Sheet
updateTime newTime sheet =
    { sheet | time = HasTime newTime }


updateTimeZone : Time.Zone -> Sheet -> Sheet
updateTimeZone newZone sheet =
    { sheet | zone = HasTZ newZone }


---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
--------------- PROJECT AND PROJECT TYPE MANIPULATION ---------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Adds a ProjectType to the Sheet.
-}
addProjectType : String -> ProjectType.Breakdown -> Sheet -> Sheet
addProjectType name breakdown sheet =
    let
        newProjType =
            ProjectType.fromValues name breakdown

        newKey =
            getNewIncrementedDictKey sheet.projTypes
    in
    { sheet | projTypes = Dict.insert newKey newProjType sheet.projTypes }


{-| Edits a ProjectType and saves the edit back into the Sheet.

Will return an error if the ProjectType cannot be found.

-}
editProjectType : String -> ProjectType.ID -> Sheet -> Result Err Sheet
editProjectType newName projTypeID sheet =
    case Dict.get projTypeID sheet.projTypes of
        Nothing ->
            Err ProjTypeNotFound

        Just projType ->
            let
                newProjType =
                    ProjectType.edit newName projType
            in
            Ok { sheet | projTypes = Dict.insert projTypeID newProjType sheet.projTypes }


{-| Adds a Project to the Sheet.

Will return an Err if the given Project Type ID does not exist.

-}
addProject : String -> ProjectType.ID -> Project.MonetaryValue -> Sheet -> Result Err Sheet
addProject name projTypeID mValue sheet =
    let
        newProj =
            Project.fromValues name projTypeID mValue

        newKey =
            getNewIncrementedDictKey sheet.projects
    in
    if Dict.member projTypeID sheet.projTypes then
        Ok { sheet | projects = Dict.insert newKey newProj sheet.projects }

    else
        Err ProjTypeNotFound


{-| Edits a Project and saves the edit back into the Sheet.

Will return an error if the Project cannot be found.

-}
editProject : String -> Project.MonetaryValue -> Project.ID -> Sheet -> Result Err Sheet
editProject newName newMonValue projectID sheet =
    case Dict.get projectID sheet.projects of
        Nothing ->
            Err ProjNotFound

        Just project ->
            let
                newProject =
                    Project.edit newName newMonValue project
            in
            Ok { sheet | projects = Dict.insert projectID newProject sheet.projects }


{-| Deletes a project (and everything inside like blocks) from a Sheet.

Will return an error if the project cannot be found.

-}
deleteProject : Project.ID -> Sheet -> Result Err Sheet
deleteProject projID sheet =
    if Dict.member projID sheet.projects then
        Ok { sheet | projects = Dict.remove projID sheet.projects }

    else
        Err ProjNotFound



---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
------------------------- CURRENT BLOCK STUFF -----------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Starts a new block.

It creates a new block in the currentBlock, and puts the time this
function was used as the starting time.

If the time has not been initialised, it will return an error.

-}
startCurrentBlock : Project.ID -> Subtask.ID -> Sheet -> Result Err Sheet
startCurrentBlock projectID subtaskID sheet =
    case sheet.time of
        NoTime ->
            Err TimeNotInitialised

        HasTime time ->
            Ok
                { sheet
                    | currentBlock =
                        Just
                            { start = time
                            , projectID = projectID
                            , subtaskID = subtaskID
                            }
                }


{-| Ends the current block and attaches it to the record of blocks.

The time this function is called will become that block's end time.

It will return an error if the various parts of data required for this
to work are not there.

-}
endCurrentBlock : Sheet -> Result Err Sheet
endCurrentBlock sheet =
    -- make sure the current block is there
    case sheet.currentBlock of
        Nothing ->
            Err NoCurrentBlock

        Just currentBlock ->
            -- make sure time is initialised
            case sheet.time of
                NoTime ->
                    Err TimeNotInitialised

                HasTime time ->
                    -- make sure the start and end time do not overlap
                    if time == currentBlock.start then
                        Err BlockTimeOverlap

                    else
                        let
                            projID =
                                currentBlock.projectID
                        in
                        case Dict.get projID sheet.projects of
                            Nothing ->
                                Err ProjNotFound

                            Just proj ->
                                let
                                    newBlock =
                                        Block.fromValues currentBlock.start time currentBlock.subtaskID

                                    newProj =
                                        Project.addBlock newBlock proj
                                in
                                Ok
                                    { sheet
                                        | currentBlock = Nothing
                                        , projects = Dict.insert projID newProj sheet.projects
                                    }


---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
-------------------------- QUERY STUFF ------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Gets all Projects that have a certain ProjectType ID.

Returns an error if the ProjectType cannot be found in the database.
-}
toProjectsFilteredByType : ProjectType.ID -> Sheet -> Result Err (Dict Int Project)
toProjectsFilteredByType projTypeID sheet =
    if Dict.member projTypeID sheet.projTypes then
        Ok <| internalToProjectsByType projTypeID sheet
    else
        Err ProjTypeNotFound


{-| Gets all Projects that have a certain ProjectType ID with
only the blocks that have a certain Subtask ID.
-}
toProjectsFilteredBySubtask : ProjectType.ID 
                    -> Subtask.ID
                    -> Sheet
                    -> Result Err (Dict Project.ID Project)
toProjectsFilteredBySubtask projTypeID subtaskID sheet =
    let
        compileResults =
            \k maybeProj results ->
                case maybeProj of
                    Nothing ->
                        results

                    Just p ->
                        Dict.insert k p results
    in
    case Dict.get projTypeID sheet.projTypes of
        Nothing -> Err ProjTypeNotFound
        Just projType ->
            if not <| ProjectType.hasSubtask subtaskID projType then
                Err SubtaskIDNotFound
            else
                sheet
                    |> internalToProjectsByType projTypeID
                    |> Dict.map (\_ p -> Project.filterBlocksBySubtask subtaskID p)
                    |> Dict.foldl compileResults Dict.empty
                    |> Ok


---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
--------------------------- MASS EDIT -------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Goes through all of the projects and replaces a specific Subtask ID with another one.
-}
replaceSubtaskIDs : ProjectType.ID -> Subtask.ID -> Subtask.ID -> Sheet -> Sheet
replaceSubtaskIDs projTypeID wantedID replacementID sheet =
    let
        replaceIDs = (\_ proj -> 
            if Project.hasProjectTypeID projTypeID proj then
                Project.replaceSubtaskIDs wantedID replacementID proj
            else
                proj
            )
    in
    { sheet | projects  = Dict.map replaceIDs sheet.projects}


---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------- HELPER ---------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Gets a fresh key for a Dict by looking for the highest existing
key and providing a incrementing on that.

If the Dict is empty, then it returns 0.

This is important for adding a new item to an incremented Dict.

-}
getNewIncrementedDictKey : Dict Int v -> Int
getNewIncrementedDictKey dict =
    dict
        |> Dict.keys
        |> List.maximum
        |> Maybe.map (\n -> n + 1)
        |> Maybe.withDefault 0


internalToProjectsByType : ProjectType.ID -> Sheet -> Dict Int Project
internalToProjectsByType projTypeID sheet =
    Dict.filter (\_ proj -> Project.hasProjectTypeID projTypeID proj) sheet.projects