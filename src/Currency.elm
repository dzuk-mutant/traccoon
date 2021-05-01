module Currency exposing (Currency, Value, Code, valueToPlainString)


{-| ie. JPY, USD, GBP, EUR, NOK, etc.
-}
type alias Code = String

{-| A currency for a monetary value.

Currency type exists for more pleasant-looking monetary value formatting.
-}
type alias Currency =
    { prefix : Maybe String
    , symbol : Maybe String
    , code : String
    }

{-| A monetary value that is in the currency defined by the currency code.
-}
type alias Value =
    { bigNumbers : Int
    , decimalPlaces : Int
    , currencyCode : Code
    }


-------------------------------------------------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------


{-| Takes a monetary value and sees if it can automatically assign
it a currency stored in this app for cleaner formatting.
-}
matchCurrency : Value -> Maybe Currency
matchCurrency value =
    let
        currencies = [gbp, eur, usd, jpy, aud, cad, nzd, hkd, krw]

        -- case-insensitive match
        match = (\currency -> currency.code == String.toUpper value.currencyCode)
    in
        List.head <| List.filter match currencies


{-| Creates a neat human-readable string out of a Value.

If the app detects a currency code it has stored, then it will prettify the currency.
-}
valueToPlainString : Value -> String
valueToPlainString value =
    let
        number =
            String.fromInt value.bigNumbers
            ++ "."
            ++ String.fromInt value.decimalPlaces
    in
        case matchCurrency value of
            Nothing ->
                value.currencyCode
                ++ " "
                ++ number
            Just currency ->
                ( Maybe.withDefault "" currency.prefix )
                ++ ( Maybe.withDefault "" currency.symbol )
                ++ number


-------------------------------------------------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------


gbp : Currency
gbp =
    { prefix = Nothing
    , symbol = Just "£"
    , code = "GBP"
    }

eur : Currency
eur =
    { prefix = Nothing
    , symbol = Just "€"
    , code = "EUR"
    }

usd : Currency
usd =
    { prefix = Just "US"
    , symbol = Just "$"
    , code = "USD"
    }

jpy : Currency
jpy =
    { prefix = Nothing
    , symbol = Just "¥"
    , code = "JPY"
    }

aud : Currency
aud =
    { prefix = Just "A"
    , symbol = Just "$"
    , code = "AUD"
    }

cad : Currency
cad =
    { prefix = Just "C"
    , symbol = Just "$"
    , code = "CAD"
    }

nzd : Currency
nzd =
    { prefix = Just "NZ"
    , symbol = Just "$"
    , code = "NZD"
    }

hkd : Currency
hkd =
    { prefix = Just "HK"
    , symbol = Just "$"
    , code = "HKD"
    }

krw : Currency
krw =
    { prefix = Nothing
    , symbol = Just "₩"
    , code = "KRW"
    }