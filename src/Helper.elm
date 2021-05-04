module Helper exposing (millisToHours)


{-| Converts an int representing milliseconds
into a float representing hours.
-}
millisToHours : Int -> Float
millisToHours millis =
    toFloat millis / 3600000