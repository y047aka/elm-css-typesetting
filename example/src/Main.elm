module Main exposing (main)

import Browser
import Css exposing (..)
import Css.Extra exposing (..)
import Css.Global exposing (Snippet, children, everything)
import Css.Palette exposing (palette, paletteWithBorder)
import Css.Typography as Typography exposing (Typography, typography)
import DesignToken.Palette as Palette
import Emaki.Props as Props exposing (Props)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { typography : Typography
    , fontSize : Float
    , letterSpacing : Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { typography =
            Typography.init
                |> Typography.setFontFamilies [ "sans-serif" ]
                |> Typography.setFontSize (px 16)
                |> Typography.setTextDecoration Css.none
      , fontSize = 16
      , letterSpacing = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateProps (Model -> Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateProps updater ->
            ( updater model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    main_
        [ css
            [ padding (Css.em 1)
            , before
                [ property "content" "''"
                , position absolute
                , property "inset" "0"
                , zIndex (int -2)
                , property "background" """
radial-gradient(at 80% 90%, hsl(200, 100%, 90%), hsl(200, 100%, 90%) 40%, transparent 40%),
radial-gradient(at 70% -5%, hsl(300, 100%, 90%), hsl(300, 100%, 90%) 30%, transparent 40%),
radial-gradient(at 5% 0%, hsl(200, 100%, 80%), hsl(200, 100%, 80%) 50%, transparent 50%)"""
                ]
            , after
                [ property "content" "''"
                , position absolute
                , property "inset" "0"
                , zIndex (int -1)
                , property "-webkit-backdrop-filter" "blur(100px) contrast(1.2)"
                , property "backdrop-filter" "blur(100px) contrast(1.2)"
                ]
            ]
        ]
        [ resetCSS
        , article
            [ css
                [ displayFlex
                , flexDirection column
                , rowGap (Css.em 0.5)
                ]
            ]
            [ h2 [] [ text "Typography" ]
            , playground
                { preview = p [ css [ typography model.typography ] ] [ text "Hello World!" ]
                , props =
                    [ Props.FieldSet "font-family"
                        [ Props.select
                            { value = model.typography.fontFamilies |> String.concat
                            , options = [ Css.sansSerif.value, Css.serif.value ]
                            , onChange =
                                (\fontFamily m ->
                                    { m
                                        | typography =
                                            m.typography
                                                |> (case fontFamily of
                                                        "sans-serif" ->
                                                            Typography.setFontFamilies [ "sans-serif" ]

                                                        "serif" ->
                                                            Typography.setFontFamilies [ "serif" ]

                                                        _ ->
                                                            identity
                                                   )
                                    }
                                )
                                    >> UpdateProps
                            }
                        ]
                    , Props.FieldSet "font-size"
                        [ Props.counter
                            { value = model.fontSize
                            , toString = \value -> String.fromFloat value ++ "px"
                            , onClickPlus =
                                UpdateProps
                                    (\m ->
                                        { m
                                            | fontSize = m.fontSize + 1
                                            , typography = m.typography |> Typography.setFontSize (px (m.fontSize + 1))
                                        }
                                    )
                            , onClickMinus =
                                UpdateProps
                                    (\m ->
                                        { m
                                            | fontSize = m.fontSize - 1
                                            , typography = m.typography |> Typography.setFontSize (px (m.fontSize - 1))
                                        }
                                    )
                            }
                        ]
                    , Props.FieldSet "text-decoration"
                        [ Props.radio
                            { value = model.typography.textDecoration |> Maybe.map .value |> Maybe.withDefault "-"
                            , options = [ Css.none.value, Css.underline.value ]
                            , onChange =
                                (\decoration m ->
                                    { m
                                        | typography =
                                            m.typography
                                                |> (case decoration of
                                                        "none" ->
                                                            Typography.setTextDecoration Css.none

                                                        "underline" ->
                                                            Typography.setTextDecoration Css.underline

                                                        _ ->
                                                            identity
                                                   )
                                    }
                                )
                                    >> UpdateProps
                            }
                        ]
                    , Props.FieldSet "letter-spacing"
                        [ Props.counter
                            { value = model.letterSpacing
                            , toString = \value -> String.fromFloat value ++ "em"
                            , onClickPlus =
                                UpdateProps
                                    (\m ->
                                        { m
                                            | letterSpacing = ((m.letterSpacing * 100) + 1) / 100
                                            , typography = m.typography |> Typography.setLetterSpacing (Css.em (((m.letterSpacing * 100) + 1) / 100))
                                        }
                                    )
                            , onClickMinus =
                                UpdateProps
                                    (\m ->
                                        { m
                                            | letterSpacing = ((m.letterSpacing * 100) - 1) / 100
                                            , typography = m.typography |> Typography.setLetterSpacing (Css.em (((m.letterSpacing * 100) - 1) / 100))
                                        }
                                    )
                            }
                        ]
                    ]
                }
            ]
        ]


playground :
    { preview : Html msg
    , props : List (Props msg)
    }
    -> Html msg
playground { preview, props } =
    section
        [ css
            [ padding4 (Css.em 0.5) (Css.em 0.5) (Css.em 0.5) (Css.em 1.5)
            , borderRadius (Css.em 1.5)
            , display grid
            , property "grid-template-columns" "1fr 25em"
            , columnGap (Css.em 1.5)
            , fontSize (px 14)
            , paletteWithBorder (border3 (px 1) solid) Palette.playground
            , property "-webkit-backdrop-filter" "blur(300px)"
            , property "backdrop-filter" "blur(300px)"
            , property "box-shadow" "0 5px 20px hsl(0, 0%, 0%, 0.05)"
            ]
        ]
        [ div [ css [ displayFlex, flexDirection column, justifyContent center ] ]
            [ preview ]
        , div
            [ css
                [ padding (Css.em 0.5)
                , displayFlex
                , flexDirection column
                , rowGap (Css.em 0.5)
                , borderRadius (Css.em 1)
                , palette Palette.propsPanel
                , children
                    [ everything
                        [ padding (Css.em 0.75)
                        , displayFlex
                        , flexDirection column
                        , rowGap (Css.em 0.5)
                        , borderRadius (Css.em 0.5)
                        , palette Palette.propsField
                        ]
                    ]
                ]
            ]
            (List.map Props.render props)
        ]



-- RESET CSS


resetCSS : Html msg
resetCSS =
    let
        where_ : String -> List Style -> Snippet
        where_ selector_ styles =
            Css.Global.selector (":where(" ++ selector_ ++ ")") styles
    in
    Css.Global.global
        [ Css.Global.selector "*, ::before, ::after"
            [ boxSizing borderBox
            , property "-webkit-font-smoothing" "antialiased"
            ]
        , Css.Global.everything
            [ margin zero ]
        , where_ ":root"
            [ fontFamily sansSerif ]
        ]
