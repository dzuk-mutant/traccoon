module Helper exposing (colorFromType, filterDictMaybes, getNewIncrementedDictKey)

import Color exposing (Color)
import Color.Convert
import Css exposing (Style)
import Dict exposing (Dict)


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


{-| Takes a Dict with Maybe values and filters it into a
Dict that only has actual values.
-}
filterDictMaybes : Dict comparable (Maybe v) -> Dict comparable v
filterDictMaybes dict =
    let
        compileResults =
            \k maybeProj results ->
                case maybeProj of
                    Nothing ->
                        results

                    Just p ->
                        Dict.insert k p results
    in
    Dict.foldl compileResults Dict.empty dict


{-| Creates a Css style equivalent to `color` that can accept a Color type instead of a hex string.
-}
colorFromType : Color -> Style
colorFromType colorData =
    colorData
        |> Color.Convert.colorToHex
        |> Css.hex
        |> Css.color
