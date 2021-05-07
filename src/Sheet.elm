module Sheet exposing
    ( Sheet
    , Err(..)
    , Time(..)
    , TimeZone(..)

    , init

    , updateTime
    , updateTimeZone
    
    , addProjectType
    , editProjectType
    , addSubtask
    , deleteSubtask

    , addProject
    , editProject
    , deleteProject

    , startCurrentBlock
    , endCurrentBlock
    
    , toProjectsFilteredBySubtask
    , toProjectsFilteredByType
    , toProjectsFilteredByTimeframe
    )

{-| The module that handles the Sheet - the core data structure for Traccoon.

# Types
@docs Sheet, Err

# Init and updating the time
@docs init, updateTime, updateTimeZone

# Creating and editing data points
## ProjectTypes
@docs addProjectType, editProjectType, addSubtask, deleteSubtask

## Projects
@docs addProject, editProject, deleteProject

# Creating new blocks
@docs startCurrentBlock, endCurrentBlock

# Filtering data
@docs toProjectsFilteredByType, toProjectsFilteredBySubtask
-}

import Block
import Dict exposing (Dict)
import Helper exposing (filterDictMaybes)
import Project exposing (Project)
import ProjectType exposing (ProjectType)
import Subtask exposing (Subtask)
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
- BlockTimeOverlap - A block has been stopped at the same time as it's start, which cannot be allowed to happen.
- ProjNotFound - Tried to access, use or edit a Project that's not there.
- ProjTypeNotFound - Tried to access, use or edit a ProjectType that's not there.
- ProjTypeErr - A problem happened while trying to manipulate a ProjectType.

-}
type Err
    = TimeNotInitialised
    | NoCurrentBlock
    | BlockTimeOverlap

    | ProjNotFound
    
    | ProjTypeNotFound
    | ProjTypeErr ProjectType.Err

    


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
--------------------- PROJECT TYPE MANIPULATION ---------------------
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
            Helper.getNewIncrementedDictKey sheet.projTypes
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
                    ProjectType.editName newName projType
            in
            Ok { sheet | projTypes = Dict.insert projTypeID newProjType sheet.projTypes }


{-| Adds a Subtask to a particular ProjectType.

Returns an error if the ProjectType doesn't exist or isn't subtasked.
-}
addSubtask : ProjectType.ID -> Subtask -> Sheet -> Result Err Sheet
addSubtask projTypeID newSubtask sheet =
    case getProjType projTypeID sheet of
        Err e -> Err e
        Ok projType ->
            case ProjectType.addSubtask newSubtask projType of
                Err e -> Err ( ProjTypeErr e )
                Ok updatedProjType ->
                    let
                        updatedProjTypes = Dict.insert projTypeID updatedProjType sheet.projTypes
                    in
                        Ok { sheet | projTypes = updatedProjTypes }


{-| Deletes a Subtask and replaces all instances of that Subtask
with an alternative that does exist.

Will return errors if the Subtask or ProjectTypeIDs are not there.
-}
deleteSubtask : ProjectType.ID -> Subtask.ID -> Subtask.ID -> Sheet -> Result Err Sheet
deleteSubtask projTypeID subtaskToRemove replacementSubtask sheet =
    case getProjType projTypeID sheet of
        Err e -> Err e
        Ok projType ->
            case ProjectType.deleteSubtask subtaskToRemove projType of
                Err e -> Err ( ProjTypeErr e )
                Ok updatedProjType ->
                    let
                        updatedProjTypes = Dict.insert projTypeID updatedProjType sheet.projTypes
                    in
                        sheet
                        |> replaceSubtaskIDs projTypeID subtaskToRemove replacementSubtask
                        |> (\s -> {s | projTypes = updatedProjTypes })
                        |> Ok


---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
------------------------ PROJECT MANIPULATION -----------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Adds a Project to the Sheet.

Will return an Err if the given Project Type ID does not exist.

-}
addProject : String -> ProjectType.ID -> Project.MonetaryValue -> Sheet -> Result Err Sheet
addProject name projTypeID mValue sheet =
    let
        newProj =
            Project.fromValues name projTypeID mValue

        newKey =
            Helper.getNewIncrementedDictKey sheet.projects
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
----------------------- CURRENT BLOCK STUFF -------------------------
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
    if hasProjectTypeID projTypeID sheet then
        Ok <| internalToProjectsFilteredByType projTypeID sheet
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
    case Dict.get projTypeID sheet.projTypes of
        Nothing -> Err ProjTypeNotFound
        Just projType ->
            if not <| ProjectType.hasSubtask subtaskID projType then
                Err ( ProjTypeErr ProjectType.SubtaskNotFound )
            else
                sheet
                    |> internalToProjectsFilteredByType projTypeID
                    |> Dict.map (\_ p -> Project.filterBlocksBySubtask subtaskID p)
                    |> filterDictMaybes
                    |> Ok


{-| Returns all projects that have blocks that are in a certain timeframe.
The returned projects' blocks are also filtered based on whether they
are in that time frame.
-}
toProjectsFilteredByTimeframe : Time.Posix 
                            -> Time.Posix
                            -> Sheet
                            -> Dict Project.ID Project
toProjectsFilteredByTimeframe startTime endTime sheet =
    sheet.projects
    |> Dict.map (\_ p -> Project.filterBlocksByTimeframe startTime endTime p)
    |> filterDictMaybes

---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------- HELPER ---------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Gets a projectType from the Sheet. Will return an Err if it can't
be found.
-}
getProjType : ProjectType.ID -> Sheet -> Result Err ProjectType
getProjType projTypeID sheet =
    case Dict.get projTypeID sheet.projTypes of
        Nothing -> Err ProjTypeNotFound
        Just projType -> Ok projType

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

{-| The actual function that gets all of the Projects that have a certain ProjectType.
-}
internalToProjectsFilteredByType : ProjectType.ID -> Sheet -> Dict Int Project
internalToProjectsFilteredByType projTypeID sheet =
    Dict.filter (\_ proj -> Project.hasProjectTypeID projTypeID proj) sheet.projects


{-| Checks if a ProjectType ID exists.
-}
hasProjectTypeID : ProjectType.ID -> Sheet -> Bool
hasProjectTypeID projTypeID sheet =
    Dict.member projTypeID sheet.projTypes

