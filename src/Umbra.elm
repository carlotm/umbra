module Umbra exposing (main)

import Browser
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



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
    }


type ButtonLevel
    = Primary
    | Secondary


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


type ShadowParam
    = XOffset
    | YOffset
    | Blur
    | Spread
    | Color


type Section
    = Tools
    | Shadows
    | Output



----------------------------------------------
-- Update
----------------------------------------------


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetShape newShape ->
            { model | shape = newShape }

        SetSize newSize ->
            { model | size = newSize }

        SetColor newColor ->
            { model | color = newColor }

        SelectShadow id ->
            { model | selectedShadowId = id }

        ShadowSettingsClose ->
            { model | selectedShadowId = "" }

        SetXOffset offset ->
            updateSelectedShadow model XOffset offset

        SetYOffset offset ->
            updateSelectedShadow model YOffset offset

        SetBlur blur ->
            updateSelectedShadow model Blur blur

        SetSpread spread ->
            updateSelectedShadow model Spread spread

        SetShadowColor color ->
            updateSelectedShadow model Color color

        DeleteSelectedShadow ->
            { model
                | selectedShadowId = ""
                , shadows = List.filter (\s -> s.id /= model.selectedShadowId) model.shadows
            }

        AddShadow ->
            { model
                | shadows = makeShadow model :: model.shadows
                , selectedShadowId = ""
            }

        ExportCSS ->
            { model
                | css = Just (buildBoxShadow model.shadows)
                , selectedShadowId = ""
            }

        Guybrush ->
            guybrush

        CloseExportModal ->
            { model
                | css = Nothing
                , selectedShadowId = ""
            }



----------------------------------------------
-- View
----------------------------------------------


view : Model -> Html Msg
view model =
    div [ class "App" ]
        [ viewSection Tools (viewTools model) (Just viewToolsFooter)
        , viewSection Output (viewOutput model) Nothing
        , viewSection Shadows (viewShadows model) (Just viewShadowsFooter)
        , case model.css of
            Nothing ->
                text ""

            Just css ->
                viewModalExport css
        ]



----------------------------------------------
-- Main
----------------------------------------------


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }



----------------------------------------------
-- Helpers
----------------------------------------------


viewSection : Section -> List (Html Msg) -> Maybe (Html Msg) -> Html Msg
viewSection sectionType children footer =
    let
        ( className, title, subtitle ) =
            case sectionType of
                Tools ->
                    ( "Tools", "Base settings", "The base element settings." )

                Shadows ->
                    ( "Shadows", "Shadows list", "The list of all currently visible shadows." )

                Output ->
                    ( "Output", "Result", "See the result real time below." )

        sectionFooter =
            case footer of
                Just f ->
                    div [ class "Section-footer" ] [ f ]

                Nothing ->
                    text ""
    in
    section [ class ("Section " ++ className) ]
        [ header [ class "Section-header" ]
            [ h2 [] [ text title ]
            , p [] [ text subtitle ]
            ]
        , div [ class "Section-content" ] children
        , sectionFooter
        ]


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
                [ code [] [ text ("box-shadow: " ++ css) ] ]
            ]
        ]


viewTools : Model -> List (Html Msg)
viewTools model =
    [ div [ class "FormField is-shape" ]
        [ p [ class "FormField-title" ] [ text "Shape" ]
        , div [ class "FormField-element" ] (viewInputShape model.shape)
        ]
    , div [ class "FormField is-size" ]
        [ p [ class "FormField-title" ] [ text "Size" ]
        , div [ class "FormField-element" ] (viewInputSize model.size)
        ]
    , div [ class "FormField is-color" ]
        [ p [ class "FormField-title" ] [ text "Color" ]
        , div [ class "FormField-element" ] (viewInputColor model.color)
        ]
    ]


viewToolsFooter : Html Msg
viewToolsFooter =
    div [ class "Buttons" ]
        [ btn "Export CSS" ExportCSS Primary
        , btn "Guybrush!" Guybrush Secondary
        ]


btn : String -> Msg -> ButtonLevel -> Html Msg
btn t e l =
    let
        levelClass =
            case l of
                Primary ->
                    "primary"

                Secondary ->
                    "secondary"
    in
    button [ type_ "button", class ("Button is-" ++ levelClass), onClick e ] [ text t ]


viewShadows : Model -> List (Html Msg)
viewShadows model =
    List.map (\s -> viewShadowItem s model.selectedShadowId) model.shadows


viewShadowsFooter : Html Msg
viewShadowsFooter =
    div [] [ btn "Add a shadow" AddShadow Primary ]


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
    , viewShadowSettings (fetchSelectedShadow model)
    ]


viewInputShape : String -> List (Html Msg)
viewInputShape value_ =
    let
        shapes =
            [ ( "sqr", "Square", FeatherIcons.square )
            , ( "rnd", "Circle", FeatherIcons.circle )
            ]
    in
    List.map
        (\( v, l, i ) ->
            div []
                [ input
                    [ id ("shape-" ++ v)
                    , type_ "radio"
                    , name "shapes"
                    , value v
                    , onInput SetShape
                    , checked (value_ == v)
                    ]
                    []
                , label [ for ("shape-" ++ v) ]
                    [ text l
                    , i |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml []
                    ]
                ]
        )
        shapes


viewInputSize : String -> List (Html Msg)
viewInputSize value_ =
    [ input [ onInput SetSize, type_ "range", Html.Attributes.min "1", Html.Attributes.max "100" ] []
    , p [] [ text (value_ ++ "px") ]
    ]


viewInputColor : String -> List (Html Msg)
viewInputColor value_ =
    [ label [ style "background-color" value_ ]
        [ input [ onInput SetColor, type_ "color", value value_ ] []
        ]
    ]


viewShadowSettings : Maybe Shadow -> Html Msg
viewShadowSettings maybeShadow =
    case maybeShadow of
        Just s ->
            div [ class "Modal is-shadow" ]
                [ div [ class "Modal-window" ]
                    [ div [ class "Modal-header" ]
                        [ h3 [ class "Modal-title" ] [ text ("Shadow #" ++ s.id ++ " settings") ]
                        , button [ type_ "button", class "Modal-close", onClick ShadowSettingsClose ]
                            [ FeatherIcons.x |> FeatherIcons.toHtml [] ]
                        ]
                    , div [ class "Modal-content" ]
                        [ label [ class "FormFieldCompact" ]
                            [ text "Horizontal offset"
                            , input [type_ "number", value s.xOffset, onInput SetXOffset ] []
                            ]
                        , label [ class "FormFieldCompact" ]
                            [ text "Vertical offset"
                            , input [type_ "number", value s.yOffset, onInput SetYOffset ] []
                            ]
                        , label [ class "FormFieldCompact" ]
                            [ text "Blur"
                            , input [type_ "number", value s.blur, onInput SetBlur ] []
                            ]
                        , label [ class "FormFieldCompact" ]
                            [ text "Spread"
                            , input [type_ "number", value s.spread, onInput SetSpread ] []
                            ]
                        , label [ class "FormFieldCompact is-color" ]
                            [ text "Color"
                            , div [ style "background-color" s.color ] [ ]
                            , input [ onInput SetShadowColor, type_ "color", value s.color ] []
                            ]
                        , button [ class "Button is-danger is-center has-icon", onClick DeleteSelectedShadow ]
                            [ text "Delete"
                            , FeatherIcons.trash |> FeatherIcons.withSize 14 |> FeatherIcons.toHtml []
                            ]
                        ]
                    ]
                ]

        Nothing ->
            text ""


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

        Color ->
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
        "#000000"
        [ Shadow "2" "5" "5" "0" "0" "#ff0000"
        , Shadow "1" "10" "10" "0" "0" "#00ff00"
        ]
        ""
        Nothing


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
