module Helper exposing (millisToHours, getNewIncrementedDictKey)


import Dict exposing (Dict)

{-| Converts an int representing milliseconds
into a float representing hours.
-}
millisToHours : Int -> Float
millisToHours millis =
    toFloat millis / 3600000


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
