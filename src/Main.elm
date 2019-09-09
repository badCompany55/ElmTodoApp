module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { todos : List Todo
    , newTodo : String
    }


type alias Todo =
    { id : Int
    , todo : String
    , complete : Bool
    }


type Msg
    = CapInput String
    | AddTodo
    | ToggleComplete Todo
    | DeleteComplete


reverseSort a b =
    compare b a


changeToString : Bool -> String
changeToString tf =
    if tf == True then
        "True"

    else
        "False"


toggleComplete : Model -> Todo -> Model
toggleComplete model todo =
    let
        newTodos todo_ =
            if todo_.id == todo.id then
                if todo_.complete == False then
                    { todo_ | complete = True }

                else
                    { todo_ | complete = False }

            else
                todo_
    in
    { model | todos = List.map newTodos model.todos }


changeChecked : Todo -> Bool
changeChecked todo =
    if todo.complete == True then
        True

    else
        False


deleteComplete : Model -> Model
deleteComplete model =
    let
        filter =
            List.filter (\x -> x.complete /= True) model.todos
    in
    { model | todos = filter }


newId model =
    if List.length model.todos > 0 then
        List.map (\x -> x.id) model.todos
            |> List.sortWith reverseSort
            |> List.take 1
            |> List.append [ 1 ]
            |> List.sum

    else
        1


newTodoCreate : Model -> Model
newTodoCreate model =
    let
        todo =
            Todo (newId model) model.newTodo False

        newTodos =
            List.append model.todos [ todo ]
    in
    { model | todos = newTodos, newTodo = "" }


clearInput : Model -> Model
clearInput model =
    { model | newTodo = "" }


update : Msg -> Model -> Model
update msg model =
    case msg of
        CapInput todoText ->
            { model | newTodo = todoText }

        AddTodo ->
            newTodoCreate model

        ToggleComplete clickedTodo ->
            toggleComplete model clickedTodo

        DeleteComplete ->
            deleteComplete model


view : Model -> Html Msg
view model =
    div [ class "cont" ]
        [ h1 [ class "title" ] [ text "Elm Todo App" ]
        , div [ class "iCont" ]
            [ input [ type_ "text", value model.newTodo, placeholder "Enter the new Todo Here", onInput CapInput ] []
            , button [ class "button", onClick AddTodo ] [ text "Add Todo" ]
            , button [ class "delete", onClick DeleteComplete ] [ text "Delete Completed" ]
            ]
        , ul [ class "todoList" ] (List.map singleTodo model.todos)
        ]


singleTodo : Todo -> Html Msg
singleTodo todoText =
    li [ class "todo" ] [ text todoText.todo, input [ type_ "checkbox", checked (changeChecked todoText), onClick (ToggleComplete todoText) ] [] ]


init : Model
init =
    Model [] ""


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
