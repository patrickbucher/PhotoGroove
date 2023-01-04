module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

type ThumbnailSize
 = Small
 | Medium
 | Large

type Msg
    = ClickedPhoto String
    | GotSelectedIndex Int
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe

view : Model -> Html Msg
view model =
    div [ class "content" ]
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
        (List.map (viewThumbnail model.selectedUrl) model.photos)
    , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ model.selectedUrl)
        ]
        []
    ]

viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser selectedSize size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (ClickedSize size)
            , checked (selectedSize == size)
            ]
            []
        , text (sizeToString size)
        ]

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"
        Medium ->
            "med"
        Large ->
            "large"

getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url
        Nothing ->
            ""

viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []

type alias Photo =
    { url : String }

type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }

initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        , { url = "4.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
    }

photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos

randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedUrl = url }, Cmd.none )
        GotSelectedIndex index ->
            ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )
        ClickedSurpriseMe ->
            ( model, Random.generate GotSelectedIndex randomPhotoPicker )
        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none)

main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }

