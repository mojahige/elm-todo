module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (checked, class, disabled, placeholder, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


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
    , completed : Bool
    }


type alias Memos =
    List Todo


type alias Model =
    { id : Int
    , input : String
    , memos : Memos
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
                    , completed = False
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
                        { memo | completed = checked }

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
                [ class "text-2xl font-bold bg-white focus:outline-none focus:shadow-outline border-4 border-blue-800 rounded-r-none rounded-l-lg py-2 px-4 block w-full appearance-none leading-normal"
                , value model.input
                , placeholder "Input TODO ⌨️"
                , onInput Input
                ]
                []
            , button
                [ class "flex-shrink-0 bg-blue-900 text-white font-bold py-2 px-4 rounded-r-lg rounded-l-none"
                , class
                    (if hasInput model.input then
                        "hover:bg-blue-800"

                     else
                        "opacity-75 cursor-not-allowed"
                    )
                , disabled (not (hasInput model.input))
                ]
                [ text "Add 😎" ]
            ]
        , if hasTodos model.memos then
            div [ class "flex justify-center mt-6" ]
                [ ul
                    [ class "divide-y divide-gray-400" ]
                    (List.indexedMap todoListComponent model.memos)
                ]

          else
            p [ class "mt-6 text-4xl font-bold text-gray-600 text-center" ] [ text "Empty todo 👀" ]
        ]


hasInput : String -> Bool
hasInput input =
    String.length input > 0


hasTodos : Memos -> Bool
hasTodos memos =
    List.length memos > 0


todoListComponent : Int -> Todo -> Html Msg
todoListComponent index todo =
    li
        [ class "p-2"
        , class
            (if index > 0 then
                "mt-2"

             else
                ""
            )
        ]
        [ todoComponent todo ]


todoComponent : Todo -> Html Msg
todoComponent todo =
    div [ class "flex items-center" ]
        [ label
            [ class "flex items-center mr-auto text-2xl font-bold" ]
            [ input
                [ type_ "checkbox"
                , checked todo.completed
                , onClick (Check todo.id (not todo.completed))
                ]
                []
            , span
                [ class "ml-2"
                , class
                    (if todo.completed then
                        "line-through"

                     else
                        ""
                    )
                ]
                [ text todo.text ]
            ]
        , button
            [ type_ "button"
            , class "ml-8 text-2xl"
            , title "Click to remove"
            , onClick (Remove todo.id)
            ]
            [ text "❎" ]
        ]
