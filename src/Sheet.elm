module Sheet exposing
    ( Sheet
    , Err
    , addProject
    , addProjectType
    , editProject
    , endCurrentBlock
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
- TimeZoneNotInitialised - The time zone has not been initialised yet and thus cannot be used.
- NoCurrentBlock - Tried to do something with the currentBlock even though there isn't one there.
- ProjNotFound - Tried to access or edit a Project that's not there.
-}
type Err
    = TimeNotInitialised
    | NoCurrentBlock
    | ProjNotFound

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
---------------------- CREATE + EDIT STUFF --------------------------
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



{-| Adds a Project to the Sheet.
-}
addProject : String -> ProjectType.ID -> Project.MonetaryValue -> Sheet -> Sheet
addProject name projTypeID mValue sheet =
    let
        newProj = Project.fromValues name projTypeID mValue
        newKey = getNewIncrementedDictKey sheet.projects
    in
        { sheet | projects = Dict.insert newKey newProj sheet.projects }




{-| Edits a project and saves the edit back into the Sheet.

Will return an error if the project cannot be found.

A Project's type cannot be changed.
-}
editProject : String -> Project.MonetaryValue -> Project.ID -> Sheet -> Result Err Sheet
editProject newName newMValue projectID sheet =
    case Dict.get projectID sheet.projects of
        Nothing -> Err ProjNotFound
        Just project ->
            let
                newProject =
                    { project
                        | name = newName
                        , monetaryValue = newMValue
                    }
            in
            Ok
                { sheet
                    | projects = Dict.insert projectID newProject sheet.projects
                }


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

