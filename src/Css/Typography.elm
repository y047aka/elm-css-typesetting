module Css.Typography exposing
    ( Typography, init
    , typography
    , setFontFamilies, setFontSize, setFontStyle, setFontWeight, setLineHeight, setLetterSpacing, setTextDecoration, setTextTransform
    )

{-|

@docs Typography, init
@docs typography
@docs setFontFamilies, setFontSize, setFontStyle, setFontWeight, setLineHeight, setLetterSpacing, setTextDecoration, setTextTransform

-}

import Css exposing (Compatible, FontSize, FontStyle, FontWeight, Length, Style, TextDecorationLine, TextTransform)


{-| -}
type alias Typography =
    { fontFamilies : List String
    , fontSize : Maybe (FontSize {})
    , fontStyle : Maybe (FontStyle {})
    , fontWeight : Maybe (FontWeight {})
    , lineHeight : Maybe (LineHeight {})
    , letterSpacing : Maybe (Length {} {})
    , textDecoration : Maybe (TextDecorationLine {})
    , textTransform : Maybe (TextTransform {})
    }


type alias LineHeight compatible =
    { compatible | value : String, lineHeight : Compatible }


{-| -}
init : Typography
init =
    { fontFamilies = []
    , fontSize = Nothing
    , fontStyle = Nothing
    , fontWeight = Nothing
    , lineHeight = Nothing
    , letterSpacing = Nothing
    , textDecoration = Nothing
    , textTransform = Nothing
    }


{-| -}
typography : Typography -> Style
typography t =
    [ case t.fontFamilies of
        [] ->
            Nothing

        _ ->
            Just (Css.fontFamilies t.fontFamilies)
    , Maybe.map Css.fontSize t.fontSize
    , Maybe.map Css.fontStyle t.fontStyle
    , Maybe.map Css.fontWeight t.fontWeight
    , Maybe.map Css.lineHeight t.lineHeight
    , Maybe.map Css.letterSpacing t.letterSpacing
    , Maybe.map Css.textDecoration t.textDecoration
    , Maybe.map Css.textTransform t.textTransform
    ]
        |> List.filterMap identity
        |> Css.batch



-- SETTER


{-| -}
setFontFamilies : List String -> Typography -> Typography
setFontFamilies families t =
    { t | fontFamilies = families }


{-| -}
setFontSize : FontSize a -> Typography -> Typography
setFontSize { value, fontSize } t =
    { t | fontSize = Just { value = value, fontSize = fontSize } }


{-| -}
setFontStyle : FontStyle a -> Typography -> Typography
setFontStyle { value, fontStyle } t =
    { t | fontStyle = Just { value = value, fontStyle = fontStyle } }


{-| -}
setFontWeight : FontWeight a -> Typography -> Typography
setFontWeight { value, fontWeight } t =
    { t | fontWeight = Just { value = value, fontWeight = fontWeight } }


{-| -}
setLineHeight : LineHeight compatible -> Typography -> Typography
setLineHeight { value, lineHeight } t =
    { t | lineHeight = Just { value = value, lineHeight = lineHeight } }


{-| -}
setLetterSpacing : Length compatible unit -> Typography -> Typography
setLetterSpacing { value, length, numericValue, unitLabel } t =
    { t | letterSpacing = Just { value = value, length = length, numericValue = numericValue, units = {}, unitLabel = unitLabel } }


{-| -}
setTextDecoration : TextDecorationLine a -> Typography -> Typography
setTextDecoration { value, textDecorationLine } t =
    { t | textDecoration = Just { value = value, textDecorationLine = textDecorationLine } }


{-| -}
setTextTransform : TextTransform compatible -> Typography -> Typography
setTextTransform { value, textTransform } t =
    { t | textTransform = Just { value = value, textTransform = textTransform } }
