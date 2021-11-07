module Externalized exposing (Block, blocksFromProjectDict, sortBlocks)

{-| As a default the data structure of Traccoon links objects
together. For instance, Blocks are intrinsically tied to Projects.

For some views however (like timelines), this is not possible.
So this module has 'externalized' variants and conversion methods.
-}

import Array
import Dict exposing (Dict)
import Project
import Block

{-| A block that has been separated from it's project.
We need to keep the project ID with it so we can still
find it on demand.
-}
type alias Block =
    { projectID : Project.ID
    , block : Block.Block
    }


{-| Converts a dictionary of projects into a list of Externalized.Blocks.
-}
blocksFromProjectDict : Dict Project.ID Project.Project -> List Block
blocksFromProjectDict projects =
    projects
    |> Dict.toList
    |> List.map blocksFromProject
    |> List.foldl (List.append) []


blocksFromProject : (Project.ID, Project.Project) -> List Block
blocksFromProject tuple =
    let
        projID = Tuple.first tuple
        project = Tuple.second tuple
    in
    Array.map (blockFromValues projID) project.blocks
    |> Array.toList

blockFromValues : Project.ID -> Block.Block -> Block
blockFromValues projID block =
    { projectID = projID
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
