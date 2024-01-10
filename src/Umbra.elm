module Umbra exposing (main)

import Browser
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
            model

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
                    ( "Tools", "Base settings", "The base element settings, affect all the shadows." )

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
    div [ class "Modal" ]
        [ div [ class "Modal-header" ]
            [ h3 [] [ text "Export CSS" ]
            , button [ type_ "button", class "Modal-close", onClick CloseExportModal ] [ text "-" ]
            ]
        , div [ class "Modal-content" ]
            [ p [] [ text css ] ]
        ]


viewTools : Model -> List (Html Msg)
viewTools model =
    [ viewFormField "Shape" model.shape viewInputShape
    , viewFormField "Size" model.size viewInputSize
    , viewFormField "Color" model.color viewInputColor
    ]


viewToolsFooter : Html Msg
viewToolsFooter =
    div []
        [ button [ type_ "button", onClick ExportCSS ] [ text "Export as CSS" ]
        , button [ type_ "button", onClick Guybrush ] [ text "Guybrush!" ]
        ]


viewShadows : Model -> List (Html Msg)
viewShadows model =
    List.map (\s -> viewShadowItem s model.selectedShadowId) model.shadows


viewShadowsFooter : Html Msg
viewShadowsFooter =
    div []
        [ button [ type_ "button", onClick AddShadow ] [ text "Add a shadow" ]
        ]


viewShadowItem : Shadow -> String -> Html Msg
viewShadowItem shadow selectedShadowId =
    let
        domId =
            "id_shadow-" ++ shadow.id
    in
    div [ class "ShadowItem" ]
        [ input [ id domId, type_ "checkbox", onInput SelectShadow, value shadow.id, checked (shadow.id == selectedShadowId) ] []
        , label [ for domId ] [ text ("Shadow #" ++ shadow.id) ]
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


viewInputShape : String -> String -> Html Msg
viewInputShape _ value_ =
    div [ class "FormField-size" ]
        [ label []
            [ input [ type_ "radio", name "shapes", value "sqr", onInput SetShape, checked (value_ == "sqr") ] []
            , text "Square"
            ]
        , label []
            [ input [ type_ "radio", name "shapes", value "rnd", onInput SetShape, checked (value_ == "rnd") ] []
            , text "Round"
            ]
        ]


viewInputXOffset : String -> String -> Html Msg
viewInputXOffset fieldId value_ =
    input [ id fieldId, onInput SetXOffset, type_ "number", value value_ ] []


viewInputYOffset : String -> String -> Html Msg
viewInputYOffset id_ value_ =
    input [ id id_, onInput SetYOffset, type_ "number", value value_ ] []


viewInputSize : String -> String -> Html Msg
viewInputSize id_ value_ =
    div [ class "FormField-size" ]
        [ input [ id id_, onInput SetSize, type_ "range", Html.Attributes.min "1", Html.Attributes.max "100" ] []
        , p [] [ text (value_ ++ "px") ]
        ]


viewInputColor : String -> String -> Html Msg
viewInputColor id_ value_ =
    input [ id id_, onInput SetColor, type_ "color", value value_ ] []


viewInputBlur : String -> String -> Html Msg
viewInputBlur id_ value_ =
    input [ id id_, onInput SetBlur, type_ "number", value value_ ] []


viewInputSpread : String -> String -> Html Msg
viewInputSpread id_ value_ =
    input [ id id_, onInput SetSpread, type_ "number", value value_ ] []


viewInputShadowColor : String -> String -> Html Msg
viewInputShadowColor id_ value_ =
    input [ id id_, onInput SetShadowColor, type_ "color", value value_ ] []


viewFormField : String -> String -> (String -> String -> Html Msg) -> Html Msg
viewFormField label_ value_ input_ =
    let
        lowerId =
            String.toLower label_

        id_ =
            "id_" ++ String.replace " " "_" lowerId
    in
    div [ class "FormField" ]
        [ label [ class "FormField-label", for id_ ] [ text label_ ]
        , div [ class "FormField-element" ] [ input_ id_ value_ ]
        ]


viewShadowSettings : Maybe Shadow -> Html Msg
viewShadowSettings maybeShadow =
    case maybeShadow of
        Just s ->
            div [ class "ShadowSettings" ]
                [ div [ class "ShadowSettings-header" ]
                    [ h3 [] [ text ("Shadow #" ++ s.id ++ " settings") ]
                    , button [ class "ShadowSettings-close", onClick ShadowSettingsClose ] [ text "-" ]
                    ]
                , viewFormField "Horizontal offset" s.xOffset viewInputXOffset
                , viewFormField "Vertical offset" s.yOffset viewInputYOffset
                , viewFormField "Blur" s.blur viewInputBlur
                , viewFormField "Spread" s.spread viewInputSpread
                , viewFormField "Color" s.color viewInputShadowColor
                , button [ onClick DeleteSelectedShadow ] [ text "Delete" ]
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
    Shadow (String.fromInt maxId) "0" "0" "0" "0" randomColor


randomColor : String
randomColor =
    "#000000"


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


initialShadows : List Shadow
initialShadows =
    [ Shadow "1" "5" "5" "0" "0" "#ff0000"
    , Shadow "2" "10" "10" "0" "0" "#00ff00"
    ]


initialModel : Model
initialModel =
    Model "sqr" "50" "#000000" initialShadows "" Nothing
