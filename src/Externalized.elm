module Externalized exposing (Block, blocksFromProjectDict, sortBlocks, filterBlocks)

{-| As a default the data structure of Traccoon links objects
together. For instance, Blocks are intrinsically tied to Projects.

For some views however (like timelines), this is not possible.
So this module has 'externalized' variants and conversion methods.

-}

import Array
import Block
import Dict exposing (Dict)
import Project
import ProjectType
import Timeframe exposing (Timeframe)

{-| A block that has been separated from it's project.
We need to keep the project ID with it so we can still
find it on demand.
-}
type alias Block =
    { projectID : Project.ID
    , projTypeID : ProjectType.ID
    , block : Block.Block
    }


{-| Converts a dictionary of projects into a list of Externalized.Blocks.
-}
blocksFromProjectDict : Dict Project.ID Project.Project -> List Block
blocksFromProjectDict projects =
    projects
        |> Dict.toList
        |> List.map blocksFromProject
        |> List.foldl List.append []


blocksFromProject : ( Project.ID, Project.Project ) -> List Block
blocksFromProject tuple =
    let
        projID =
            Tuple.first tuple

        project =
            Tuple.second tuple

        projTypeID =
            project.projTypeID
    in
    Array.map (blockFromValues projID projTypeID) project.blocks
        |> Array.toList


blockFromValues : Project.ID -> ProjectType.ID -> Block.Block -> Block
blockFromValues projID projTypeID block =
    { projectID = projID
    , projTypeID = projTypeID
    , block = block
    }


{-| Sorts Externalized.Blocks by which ones started earliest.
-}
sortBlocks : List Block -> List Block
sortBlocks blocks =
    let
        sort block1 block2 =
            Block.compare block1.block block2.block
    in
    List.sortWith sort blocks


filterBlocks : List Block -> Timeframe -> List Block
filterBlocks blocks timeframe =
    let
        partlyOverlaps : Timeframe -> Block -> Bool
        partlyOverlaps tf bl =
            Timeframe.partlyOverlaps tf bl.block.timeframe
    in
    List.filter (partlyOverlaps timeframe) blocks
