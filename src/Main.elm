module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Todo =
    { id : Int
    , text : String
    , done : Bool
    }


type alias Model =
    { id : Int
    , input : String
    , memos : List Todo
    }


init : Model
init =
    { input = ""
    , id = 0
    , memos = []
    }



-- UPDATE


type Msg
    = Input String
    | Submit
    | Remove Int
    | Check Int Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input input ->
            { model | input = input }

        Submit ->
            { model
                | input = ""
                , id = model.id + 1
                , memos =
                    { id = model.id
                    , text = model.input
                    , done = False
                    }
                        :: model.memos
            }

        Remove id ->
            { model
                | memos = List.filter (\memo -> memo.id /= id) model.memos
            }

        Check id checked ->
            let
                updateMemos memo =
                    if memo.id == id then
                        { memo | done = checked }

                    else
                        memo
            in
            { model
                | memos = List.map updateMemos model.memos
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit Submit ]
            [ input [ value model.input, onInput Input ] []
            , button [ disabled (String.length model.input < 1) ] [ text "Submit" ]
            ]
        , ul [] (List.map todoViewComponent model.memos)
        ]


todoViewComponent : Todo -> Html Msg
todoViewComponent todo =
    li [] [ labelComponent todo ]


labelComponent : Todo -> Html Msg
labelComponent todo =
    div []
        [ label
            [ if todo.done then
                class "hoge"

              else
                class ""
            ]
            [ input
                [ type_ "checkbox"
                , checked todo.done
                , onClick (Check todo.id (not todo.done))
                ]
                []
            , text todo.text
            ]
        , button [ type_ "button", onClick (Remove todo.id) ] [ text "❎" ]
        ]
