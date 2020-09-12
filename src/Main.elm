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
        [ h1 [ class "text-6xl font-black" ] [ text "Elm TODO" ]
        , Html.form
            [ class "flex flex-row"
            , onSubmit Submit
            ]
            [ input
                [ class "bg-white focus:outline-none focus:shadow-outline border-4 border-blue-800 rounded-r-none rounded-l-lg py-2 px-4 block w-full appearance-none leading-normal"
                , value model.input
                , onInput Input
                ]
                []
            , button
                [ class "bg-blue-900 text-white font-bold py-2 px-4 rounded-r-lg rounded-l-none"
                , class
                    (if String.length model.input > 0 then
                        "hover:bg-blue-800"

                     else
                        "opacity-75 cursor-not-allowed"
                    )
                , disabled (String.length model.input == 0)
                ]
                [ text "Submit" ]
            ]
        , ul [] (List.map todoListComponent model.memos)
        ]


todoListComponent : Todo -> Html Msg
todoListComponent todo =
    li [] [ todoComponent todo ]


todoComponent : Todo -> Html Msg
todoComponent todo =
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
