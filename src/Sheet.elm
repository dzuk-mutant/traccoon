module Sheet exposing
    ( Sheet
    , Err
    , addProject
    , addProjectType
    , editProject
    , editProjectType
    , endCurrentBlock
    , deleteProject
    , init
    , startCurrentBlock
    , updateTime
    , updateTimeZone
    , getProjectsByType
    )

{-| The module that handles the Sheet - the core data structure for Traccoon.
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


{-|
- TimeNotInitialised - The time has not been initialised yet and thus cannot be used.
- NoCurrentBlock - Tried to do something with the currentBlock even though there isn't one there.
- ProjNotFound - Tried to access, use or edit a Project that's not there.
- ProjTypeNotFound - Tried to access, use or edit a ProjectType that's not there.
-}
type Err
    = TimeNotInitialised
    | NoCurrentBlock
    | ProjNotFound
    | ProjTypeNotFound

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
        newProjType = ProjectType.fromValues name breakdown
        newKey = getNewIncrementedDictKey sheet.projTypes
    in
        { sheet | projTypes = Dict.insert newKey newProjType sheet.projTypes }


{-| Edits a ProjectType and saves the edit back into the Sheet.

Will return an error if the ProjectType cannot be found.
-}
editProjectType : String -> ProjectType.ID -> Sheet -> Result Err Sheet
editProjectType newName projTypeID sheet =
    case Dict.get projTypeID sheet.projTypes of
        Nothing -> Err ProjTypeNotFound
        Just projType ->
            let
                newProjType = ProjectType.edit newName projType
            in
                Ok { sheet | projTypes = Dict.insert projTypeID newProjType sheet.projTypes }


{-| Adds a Project to the Sheet.

Will return an Err if the given Project Type ID does not exist.
-}
addProject : String -> ProjectType.ID -> Project.MonetaryValue -> Sheet -> Result Err Sheet
addProject name projTypeID mValue sheet =
    let
        newProj = Project.fromValues name projTypeID mValue
        newKey = getNewIncrementedDictKey sheet.projects
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
        Nothing -> Err ProjNotFound
        Just project ->
            let
                newProject = Project.edit newName newMonValue project
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
        NoTime -> Err TimeNotInitialised
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
    case sheet.currentBlock of
        Nothing -> Err NoCurrentBlock
        Just currentBlock ->
            case sheet.time of
                NoTime -> Err TimeNotInitialised
                HasTime time ->
                    let
                        projID = currentBlock.projectID
                    in
                    case Dict.get projID sheet.projects of
                        Nothing -> Err ProjNotFound
                        Just proj ->
                            let
                                newBlock = Block.fromValues currentBlock.start time currentBlock.subtaskID
                                newProj = Project.addBlock newBlock proj
                            in
                            Ok 
                                { sheet
                                    | currentBlock = Nothing
                                    , projects = Dict.insert projID newProj sheet.projects
                                }


---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
--------------------------- MASS EDIT -------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------




---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
-------------------------- QUERY STUFF ------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------


{-| Gets all Projects that have a certain ProjectType ID.
-}
getProjectsByType : ProjectType.ID -> Sheet -> Dict Int Project
getProjectsByType projTypeID sheet =
    Dict.filter (\_ v -> v.projTypeID == projTypeID) sheet.projects




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