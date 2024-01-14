module Umbra exposing (main)

import Browser
import FeatherIcons
import Hex
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random



----------------------------------------------
-- Types
----------------------------------------------


type alias Shadow =
    { id : String
    , xOffset : String
    , yOffset : String
    , blur : String
    , spread : String
    , color : String
    }


type alias Model =
    { shape : String
    , size : String
    , color : String
    , shadows : List Shadow
    , selectedShadowId : String
    , css : Maybe String
    , someRandomColor : Color
    }


type Msg
    = SetShape String
    | SetSize String
    | SetColor String
    | SelectShadow String
    | ShadowSettingsClose
    | SetXOffset String
    | SetYOffset String
    | SetBlur String
    | SetSpread String
    | SetShadowColor String
    | DeleteSelectedShadow
    | AddShadow
    | ExportCSS
    | Guybrush
    | CloseExportModal
    | GotRandomColor Color


type ShadowParam
    = XOffset
    | YOffset
    | Blur
    | Spread
    | ShadowColor


type alias Color =
    { r : Int
    , g : Int
    , b : Int
    }



----------------------------------------------
-- Update
----------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetShape newShape ->
            ( { model | shape = newShape }, Cmd.none )

        SetSize newSize ->
            ( { model | size = newSize }, Cmd.none )

        SetColor newColor ->
            ( { model | color = newColor }, Cmd.none )

        SelectShadow id ->
            ( { model | selectedShadowId = id }, Cmd.none )

        ShadowSettingsClose ->
            ( { model | selectedShadowId = "" }, Cmd.none )

        SetXOffset offset ->
            ( updateSelectedShadow model XOffset offset, Cmd.none )

        SetYOffset offset ->
            ( updateSelectedShadow model YOffset offset, Cmd.none )

        SetBlur blur ->
            ( updateSelectedShadow model Blur blur, Cmd.none )

        SetSpread spread ->
            ( updateSelectedShadow model Spread spread, Cmd.none )

        SetShadowColor color ->
            ( updateSelectedShadow model ShadowColor color, Cmd.none )

        DeleteSelectedShadow ->
            ( { model
                | selectedShadowId = ""
                , shadows = List.filter (\s -> s.id /= model.selectedShadowId) model.shadows
              }
            , Cmd.none
            )

        AddShadow ->
            ( { model
                | shadows = makeShadow model :: model.shadows
                , selectedShadowId = ""
              }
            , Random.generate GotRandomColor randomColor
            )

        ExportCSS ->
            ( { model
                | css = Just (buildBoxShadow model.shadows)
                , selectedShadowId = ""
              }
            , Cmd.none
            )

        Guybrush ->
            ( guybrush, Cmd.none )

        CloseExportModal ->
            ( { model
                | css = Nothing
                , selectedShadowId = ""
              }
            , Cmd.none
            )

        GotRandomColor v ->
            ( { model | someRandomColor = v }, Cmd.none )



----------------------------------------------
-- View
----------------------------------------------


view : Model -> Html Msg
view model =
    div [ class "App" ]
        [ card
            [ h2 [] [ text "Tools" ]
            , p [ class "Card-subtitle" ] [ text "The base element settings." ]
            ]
            [ div [ class "Controls" ]
                (viewInputShape model.shape
                    ++ viewInputSize model.size
                    ++ viewInputColor model.color
                )
            ]
            [ div [ class "ButtonGroup" ]
                [ button
                    [ class "Button is-primary"
                    , onClick ExportCSS
                    ]
                    [ text "Export CSS" ]
                , button
                    [ class "Button is-secondary"
                    , onClick Guybrush
                    ]
                    [ text "Guybrush!" ]
                ]
            ]
        , card
            [ h2 [] [ text "Output" ]
            , p [ class "Card-subtitle" ] [ text "See the result below." ]
            ]
            (viewOutput model)
            []
        , card
            [ h2 [] [ text "Shadows list" ]
            , p [ class "Card-subtitle" ] [ text "The list of all currently visible shadows." ]
            ]
            (List.map (\s -> viewShadowItem s model.selectedShadowId) model.shadows)
            [ button
                [ class "Button is-primary"
                , onClick AddShadow
                ]
                [ text "Add a shadow" ]
            , p [ style "display" "none" ] [ text (colorToString model.someRandomColor) ]
            ]
        , renderMaybe model.css viewModalExport
        ]



----------------------------------------------
-- Main
----------------------------------------------


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



----------------------------------------------
-- Helpers
----------------------------------------------


viewModalExport : String -> Html Msg
viewModalExport css =
    div [ class "Modal is-fullscreen" ]
        [ div [ class "Modal-window" ]
            [ div [ class "Modal-header" ]
                [ h3 [ class "Modal-title" ] [ text "Export CSS" ]
                , button [ type_ "button", class "Modal-close", onClick CloseExportModal ]
                    [ FeatherIcons.x |> FeatherIcons.toHtml [] ]
                ]
            , div [ class "Modal-content" ]
                [ code [ class "CSSExport" ] [ text ("box-shadow: " ++ css) ] ]
            ]
        ]


viewShadowItem : Shadow -> String -> Html Msg
viewShadowItem shadow selectedShadowId =
    let
        domId =
            "id_shadow-" ++ shadow.id
    in
    div [ class "ShadowItem" ]
        [ input [ id domId, type_ "checkbox", onInput SelectShadow, value shadow.id, checked (shadow.id == selectedShadowId) ] []
        , label [ for domId ]
            [ text ("Shadow #" ++ shadow.id)
            , FeatherIcons.chevronsRight |> FeatherIcons.toHtml []
            ]
        ]


viewOutput : Model -> List (Html Msg)
viewOutput model =
    let
        rootdivStyle =
            [ style "width" (model.size ++ "px")
            , style "height" (model.size ++ "px")
            , style "background-color" model.color
            , style "box-shadow" (buildBoxShadow model.shadows)
            , classList
                [ ( "Output-rootdiv", True )
                , ( "is-round", model.shape == "rnd" )
                , ( "is-square", model.shape == "sqr" )
                ]
            ]
    in
    [ div [ class "Output-canvas" ]
        [ div rootdivStyle [] ]
    , viewMaybeShadowSettings (fetchSelectedShadow model)
    ]


viewInputShape : String -> List (Html Msg)
viewInputShape value_ =
    let
        shapes =
            [ ( "sqr", "Square" )
            , ( "rnd", "Circle" )
            ]

        radios =
            List.map
                (\( v, l ) ->
                    label [ class "Button is-secondary" ]
                        [ input
                            [ type_ "radio"
                            , name "shapes"
                            , value v
                            , onInput SetShape
                            , checked (value_ == v)
                            ]
                            []
                        , p [] [ text l ]
                        , FeatherIcons.check |> FeatherIcons.toHtml []
                        ]
                )
                shapes
    in
    [ div [ class "Control Radios" ]
        [ h3 [] [ text "Shape" ]
        , div [ class "ButtonGroup" ] radios
        ]
    ]


viewInputSize : String -> List (Html Msg)
viewInputSize value_ =
    [ div [ class "Control Range" ]
        [ label [ for "id_size" ] [ text "Size" ]
        , div [ class "Range-input" ]
            [ input
                [ id "id_size"
                , onInput SetSize
                , type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "100"
                ]
                []
            , p [] [ text (value_ ++ "px") ]
            ]
        ]
    ]


viewInputColor : String -> List (Html Msg)
viewInputColor value_ =
    [ controlColor ( "id_color", "Color" ) value_ SetColor ]


viewMaybeShadowSettings : Maybe Shadow -> Html Msg
viewMaybeShadowSettings maybeShadow =
    case maybeShadow of
        Just s ->
            viewShadowSettings s

        Nothing ->
            text ""


viewShadowSettings : Shadow -> Html Msg
viewShadowSettings s =
    div [ class "Modal is-shadow" ]
        [ div [ class "Modal-window" ]
            [ div [ class "Modal-header" ]
                [ h3 [ class "Modal-title" ] [ text ("Shadow #" ++ s.id ++ " settings") ]
                , button [ type_ "button", class "Modal-close", onClick ShadowSettingsClose ]
                    [ FeatherIcons.x |> FeatherIcons.toHtml [] ]
                ]
            , div [ class "Modal-content" ]
                [ div [ class "Controls is-multi-cols" ]
                    (viewShadowSettingsControl s)
                , button [ class "Button is-danger has-icon", onClick DeleteSelectedShadow ]
                    [ text "Delete"
                    , FeatherIcons.trash |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml []
                    ]
                ]
            ]
        ]


viewShadowSettingsControl : Shadow -> List (Html Msg)
viewShadowSettingsControl s =
    List.map
        (\( ( i, t ), v, e ) -> controlNumber ( i, t ) v e)
        [ ( ( "id_shadow_xoffset", "Horizontal Offset" ), s.xOffset, SetXOffset )
        , ( ( "id_shadow_yoffset", "Vertical offset" ), s.yOffset, SetYOffset )
        , ( ( "id_shadow_blur", "Blur" ), s.blur, SetBlur )
        , ( ( "id_shadow_spread", "Spread" ), s.spread, SetSpread )
        ]
        ++ [ controlColor ( "id_shadow_color", "Shadow color" ) s.color SetShadowColor ]


fetchSelectedShadow : Model -> Maybe Shadow
fetchSelectedShadow model =
    List.head (List.filter (\s -> s.id == model.selectedShadowId) model.shadows)


buildBoxShadow : List Shadow -> String
buildBoxShadow shadows =
    String.join ", " (List.map buildShadow shadows)


buildShadow : Shadow -> String
buildShadow s =
    String.join "px " [ s.xOffset, s.yOffset, s.blur, s.spread, s.color ]


updateSelectedShadow : Model -> ShadowParam -> String -> Model
updateSelectedShadow model param value =
    let
        updatedShadows =
            List.map
                (\s ->
                    if model.selectedShadowId == s.id then
                        updateShadow s param value

                    else
                        s
                )
                model.shadows
    in
    { model | shadows = updatedShadows }


updateShadow : Shadow -> ShadowParam -> String -> Shadow
updateShadow s param value =
    case param of
        XOffset ->
            { s | xOffset = value }

        YOffset ->
            { s | yOffset = value }

        Blur ->
            { s | blur = value }

        Spread ->
            { s | spread = value }

        ShadowColor ->
            { s | color = value }


makeShadow : Model -> Shadow
makeShadow model =
    let
        ids =
            List.map shadowId model.shadows

        maxId =
            maxOrZero ids
    in
    Shadow (String.fromInt maxId) "0" "0" "0" "0" "#000000"


maxOrZero : List Int -> Int
maxOrZero l =
    case List.maximum l of
        Just m ->
            m + 1

        Nothing ->
            0


shadowId : Shadow -> Int
shadowId s =
    case String.toInt s.id of
        Just i ->
            i

        Nothing ->
            -1


initialModel : Model
initialModel =
    Model "sqr"
        "50"
        "#ff0000"
        [ Shadow "2" "5" "5" "0" "0" "#00ff00"
        , Shadow "1" "10" "10" "0" "0" "#0000ff"
        ]
        ""
        Nothing
        { r = 200, g = 200, b = 200 }


guybrush : Model
guybrush =
    let
        skin =
            "#dd8275"

        mouth =
            "#b45844"

        hair =
            "#8c4c24"
    in
    Model "sqr"
        "20"
        "#000000"
        [ Shadow "1" "20" "0" "0" "0" skin
        , Shadow "2" "-20" "0" "0" "0" skin
        , Shadow "3" "-60" "0" "0" "0" skin
        , Shadow "4" "-20" "-20" "0" "0" skin
        , Shadow "5" "0" "-20" "0" "0" skin
        , Shadow "6" "20" "-20" "0" "0" skin
        , Shadow "7" "0" "-40" "0" "0" skin
        , Shadow "8" "-80" "20" "0" "0" skin
        , Shadow "9" "-60" "20" "0" "0" skin
        , Shadow "10" "-40" "20" "0" "0" skin
        , Shadow "11" "-20" "20" "0" "0" skin
        , Shadow "12" "0" "20" "0" "0" skin
        , Shadow "13" "20" "20" "0" "0" skin
        , Shadow "14" "40" "20" "0" "0" skin
        , Shadow "15" "-80" "40" "0" "0" skin
        , Shadow "16" "-60" "40" "0" "0" skin
        , Shadow "17" "-40" "40" "0" "0" skin
        , Shadow "18" "-20" "40" "0" "0" skin
        , Shadow "19" "0" "40" "0" "0" skin
        , Shadow "20" "20" "40" "0" "0" skin
        , Shadow "21" "-60" "60" "0" "0" skin
        , Shadow "22" "-40" "60" "0" "0" skin
        , Shadow "23" "-20" "60" "0" "0" skin
        , Shadow "24" "0" "60" "0" "0" mouth
        , Shadow "25" "20" "60" "0" "0" mouth
        , Shadow "26" "-80" "80" "0" "0" skin
        , Shadow "27" "-60" "80" "0" "0" skin
        , Shadow "28" "-40" "80" "0" "0" skin
        , Shadow "29" "-20" "80" "0" "0" skin
        , Shadow "30" "0" "80" "0" "0" skin
        , Shadow "31" "20" "80" "0" "0" skin
        , Shadow "32" "-80" "100" "0" "0" skin
        , Shadow "33" "-60" "100" "0" "0" skin
        , Shadow "34" "-40" "-80" "0" "20" hair
        , Shadow "35" "-100" "-40" "0" "20" hair
        , Shadow "36" "-80" "-60" "0" "20" hair
        , Shadow "37" "-60" "-40" "0" "20" hair
        , Shadow "38" "0" "-80" "0" "20" hair
        , Shadow "40" "20" "-60" "0" "20" hair
        , Shadow "41" "60" "-60" "0" "0" hair
        , Shadow "42" "60" "-40" "0" "0" hair
        , Shadow "43" "-40" "0" "0" "0" hair
        , Shadow "44" "-20" "-40" "0" "0" hair
        , Shadow "45" "40" "40" "0" "0" hair
        , Shadow "46" "-80" "20" "0" "20" hair
        , Shadow "47" "-140" "40" "0" "0" hair
        , Shadow "48" "-120" "40" "0" "0" hair
        , Shadow "49" "-140" "60" "0" "0" hair
        , Shadow "50" "-120" "60" "0" "0" hair
        , Shadow "51" "-80" "60" "0" "0" hair
        , Shadow "52" "-100" "60" "0" "0" hair
        , Shadow "53" "-40" "100" "0" "0" hair
        , Shadow "54" "-20" "100" "0" "0" hair
        ]
        ""
        Nothing
        { r = 200, g = 200, b = 200 }


card : List (Html Msg) -> List (Html Msg) -> List (Html Msg) -> Html Msg
card h c f =
    div [ class "Card" ]
        [ renderIf (h /= []) (div [ class "Card-header" ] h)
        , div [ class "Card-content" ] c
        , renderIf (f /= []) (div [ class "Card-footer" ] f)
        ]


controlNumber : ( String, String ) -> String -> (String -> Msg) -> Html Msg
controlNumber ( i, t ) v e =
    div [ class "Control Number" ]
        [ label [ for i ] [ text t ]
        , input [ id i, type_ "number", value v, onInput e ] []
        ]


controlColor : ( String, String ) -> String -> (String -> Msg) -> Html Msg
controlColor ( i, t ) v e =
    div [ class "Control Color" ]
        [ label [ for i ] [ text t ]
        , label
            [ class "Color-tile"
            , style "background-color" v
            , style "color" v
            ]
            [ input
                [ id i
                , onInput e
                , type_ "color"
                , value v
                ]
                []
            , text "Selected color"
            ]
        ]


renderIf : Bool -> Html Msg -> Html Msg
renderIf c e =
    if c then
        e

    else
        text ""


renderMaybe : Maybe s -> (s -> Html Msg) -> Html Msg
renderMaybe m f =
    case m of
        Nothing ->
            text ""

        Just v ->
            f v


randomColor : Random.Generator Color
randomColor =
    Random.map3
        Color
        (Random.int 0 255)
        (Random.int 0 255)
        (Random.int 0 255)


colorToString : Color -> String
colorToString c =
    "#" ++ Hex.toString c.r ++ Hex.toString c.g ++ Hex.toString c.b
