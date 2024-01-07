module Umbra exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


type alias Title =
    ( String, String )


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
    }


initialShadows : List Shadow
initialShadows =
    [ Shadow "1" "5" "5" "0" "0" "#ff0000"
    , Shadow "2" "10" "10" "0" "0" "#00ff00"
    ]


initialModel : Model
initialModel =
    Model "sqr" "50" "#000000" initialShadows ""


view : Model -> Html Msg
view model =
    div [ class "App" ]
        [ tools model
        , output model
        , shadows model
        ]


type Msg
    = SetShape String
    | SetSize String
    | SetColor String
    | SelectShadow String
    | ShadowSettingsClose


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


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }



-- Helpers


tools_title : Title
tools_title =
    ( "Base settings", "The base element settings, affect all the shadows." )


output_title : Title
output_title =
    ( "Result", "See the real time result below." )


shadows_title : Title
shadows_title =
    ( "Shadows list", "The list of all currently visible shadows." )


tools : Model -> Html Msg
tools model =
    titledSection tools_title
        "Tools"
        [ formField "Shape" (\fieldId -> shapeInput fieldId model.shape)
        , formField "Size" (\fieldId -> sizeInput fieldId model.size)
        , formField "Color" (\fieldId -> colorInput fieldId model.color)
        ]


output : Model -> Html Msg
output model =
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

        selectedShadow =
            currentShadow model
    in
    titledSection output_title
        "Output"
        [ div [ class "Output-canvas" ]
            [ div rootdivStyle [] ]
        , shadowSettings selectedShadow
        ]


shadowSettings : Maybe Shadow -> Html Msg
shadowSettings maybeShadow =
    case maybeShadow of
        Just s ->
            div [ class "ShadowSettings" ]
                [ div [ class "ShadowSettings-header" ]
                    [ h3 [] [ text ("Shadow #" ++ s.id ++ " settings") ]
                    , button [ class "ShadowSettings-close", onClick ShadowSettingsClose ] [ text "-" ]
                    ]
                , formField "Horizontal offset" (\fieldId -> xOffsetInput fieldId s)
                ]

        Nothing ->
            empty


shadows : Model -> Html Msg
shadows model =
    titledSection shadows_title "Shadows" (List.map (\s -> shadowItem s model.selectedShadowId) model.shadows)


titledSection : Title -> String -> List (Html Msg) -> Html Msg
titledSection sectionTitle baseClass children =
    let
        mainTitle =
            Tuple.first sectionTitle

        subTitle =
            Tuple.second sectionTitle
    in
    section [ class (baseClass ++ " Section") ]
        [ header [ class (baseClass ++ "-header Section-header") ]
            [ h2 [] [ text mainTitle ]
            , p [] [ text subTitle ]
            ]
        , div [ class (baseClass ++ "-content Section-content") ] children
        ]


empty : Html msg
empty =
    Html.text ""


currentShadow : Model -> Maybe Shadow
currentShadow model =
    List.head (List.filter (\s -> s.id == model.selectedShadowId) model.shadows)


formField : String -> (String -> Html Msg) -> Html Msg
formField fieldLabel formElement =
    let
        fieldID =
            "id_" ++ String.toLower fieldLabel
    in
    div [ class "FormField" ]
        [ label [ class "FormField-label", for fieldID ] [ text fieldLabel ]
        , div [ class "FormField-element" ] [ formElement fieldID ]
        ]


shapeInput : String -> String -> Html Msg
shapeInput _ shape =
    div [ class "FormField-size" ]
        [ label []
            [ input [ type_ "radio", name "shapes", value "sqr", onInput SetShape, checked (shape == "sqr") ] []
            , text "Square"
            ]
        , label []
            [ input [ type_ "radio", name "shapes", value "rnd", onInput SetShape, checked (shape == "rnd") ] []
            , text "Round"
            ]
        ]


sizeInput : String -> String -> Html Msg
sizeInput fieldId size =
    div [ class "FormField-size" ]
        [ input [ id fieldId, onInput SetSize, type_ "range", Html.Attributes.min "1", Html.Attributes.max "100" ] []
        , p [] [ text (size ++ "px") ]
        ]


colorInput : String -> String -> Html Msg
colorInput fieldId color =
    input [ id fieldId, onInput SetColor, type_ "color", value color ] []

xOffsetInput : String -> Shadow -> Html Msg
xOffsetInput fieldId shadow =
    input [ id fieldId, type_ "number", value shadow.xOffset ] []


shadowItem : Shadow -> String -> Html Msg
shadowItem shadow selectedShadowId =
    let
        domId =
            "id_shadow-" ++ shadow.id
    in
    div [ class "ShadowItem" ]
        [ input [ id domId, type_ "checkbox", onInput SelectShadow, value shadow.id, checked (shadow.id == selectedShadowId) ] []
        , label [ for domId ] [ text ("Shadow #" ++ shadow.id) ]
        ]


buildBoxShadow : List Shadow -> String
buildBoxShadow shadowsList =
    String.join ", " (List.map buildShadow shadowsList)


buildShadow : Shadow -> String
buildShadow s =
    String.join "px " [ s.xOffset, s.yOffset, s.blur, s.spread, s.color ]
