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
    , lineHeight : Float
    , letterSpacing : Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { typography =
            Typography.init
                |> Typography.setFontFamilies [ "sans-serif" ]
                |> Typography.setFontSize (px 16)
                |> Typography.setFontWeight Css.normal
                |> Typography.setLineHeight (num 1.5)
                |> Typography.setTextDecoration Css.none
      , fontSize = 16
      , lineHeight = 1.5
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
                { preview =
                    div
                        [ css
                            [ displayFlex
                            , flexDirection column
                            , rowGap (Css.em 1)
                            , typography model.typography
                            ]
                        ]
                        [ p [] [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum." ]
                        , p [] [ text "あのイーハトーヴォのすきとおった風、夏でも底に冷たさをもつ青いそら、うつくしい森で飾られたモリーオ市、郊外のぎらぎらひかる草の波。" ]
                        , p [] [ text "日本国民は正当に選挙された国会における代表者を通じて行動し、われらとわれらの子孫のために、諸国民と協和による成果と、わが国全土にわたって自由のもたらす恵沢を確保し、政府の行為によって再び戦争の惨禍が起こることのないようにすることを決意し、ここに主権が国民に存することを宣言し、この憲法を確定する。そもそも国政は国民の厳粛な信託によるものであって、その権威は国民に由来し、その権力は国民の代表者がこれを行使し、その福利は国民がこれを享受する。これは人類普遍の原理であり、この憲法は、かかる原理に基づくものである。われらはこれに反する一切の憲法、法令及び詔勅を排除する。" ]
                        , p [] [ text "日本国民は、恒久の平和を念願し、人間相互の関係を支配する崇高な理想を深く自覚するのであって、平和を愛する諸国民の公正と信義を信頼して、われらの安全と生存を保持しようと決意した。われらは平和を維持し、専制と隷従、圧迫と偏狭を地上から永遠に除去しようと努めている国際社会において、名誉ある地位を占めたいと思う。われらは全世界の国民が、ひとしく恐怖と欠乏から免れ、平和の内に生存する権利を有することを確認する。" ]
                        , p [] [ text "われらは、いずれの国家も、自国のことのみに専念して他国を無視してはならないのであって、政治道徳の法則は、普遍的なものであり、この法則に従うことは、自国の主権を維持し、他国と対等関係に立とうとする各国の責務であると信ずる。" ]
                        , p [] [ text "日本国民は、国家の名誉にかけて、全力をあげて崇高な理想と目的を達成することを誓う。" ]
                        ]
                , props =
                    [ Props.FieldSet "font-family"
                        [ Props.select
                            { value = model.typography.fontFamilies |> String.concat
                            , options = [ Css.sansSerif.value, Css.serif.value ]
                            , onChange =
                                (\fontFamily m ->
                                    { m
                                        | typography =
                                            case fontFamily of
                                                "sans-serif" ->
                                                    m.typography |> Typography.setFontFamilies [ "sans-serif" ]

                                                "serif" ->
                                                    m.typography |> Typography.setFontFamilies [ "serif" ]

                                                _ ->
                                                    m.typography
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
                    , Props.FieldSet "font-weight"
                        [ Props.radio
                            { value = model.typography.fontWeight |> Maybe.map .value |> Maybe.withDefault "-"
                            , options = [ Css.lighter.value, Css.normal.value, Css.bold.value, Css.bolder.value ]
                            , onChange =
                                (\weight m ->
                                    { m
                                        | typography =
                                            case weight of
                                                "lighter" ->
                                                    m.typography |> Typography.setFontWeight Css.lighter

                                                "normal" ->
                                                    m.typography |> Typography.setFontWeight Css.normal

                                                "bold" ->
                                                    m.typography |> Typography.setFontWeight Css.bold

                                                "bolder" ->
                                                    m.typography |> Typography.setFontWeight Css.bolder

                                                _ ->
                                                    m.typography
                                    }
                                )
                                    >> UpdateProps
                            }
                        ]
                    , Props.FieldSet "line-height"
                        [ Props.counter
                            { value = model.lineHeight
                            , toString = \value -> String.fromFloat value
                            , onClickPlus =
                                UpdateProps
                                    (\m ->
                                        { m
                                            | lineHeight = ((m.lineHeight * 10) + 1) / 10
                                            , typography = m.typography |> Typography.setLineHeight (num (((m.lineHeight * 10) + 1) / 10))
                                        }
                                    )
                            , onClickMinus =
                                UpdateProps
                                    (\m ->
                                        { m
                                            | lineHeight = ((m.lineHeight * 10) - 1) / 10
                                            , typography = m.typography |> Typography.setLineHeight (num (((m.lineHeight * 10) - 1) / 10))
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
                                            case decoration of
                                                "none" ->
                                                    m.typography |> Typography.setTextDecoration Css.none

                                                "underline" ->
                                                    m.typography |> Typography.setTextDecoration Css.underline

                                                _ ->
                                                    m.typography
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
