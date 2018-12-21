module Update

open Elmish
open Types
let createTodo (model: Model): Model =
    let newTodo = ActiveTodo ({ Id = model.CurrentKey; Text = model.CurrentInput})
    match model.CurrentInput with
    | "" -> model
    | _ ->
        { model with
            CurrentInput = "";
            TodoMap = model.TodoMap.Add(model.CurrentKey, newTodo);
            CurrentKey = model.CurrentKey + 1
        }

let switchTodo (model: Model) (id: int) =
    let todo = model.TodoMap.Item id
    match todo with
    | ActiveTodo todoItem ->
        let completedTodo = CompletedTodo({ Id = id; Text = todoItem.Text })
        let newMap = Map.add id completedTodo model.TodoMap
        { model with TodoMap = newMap }
    | CompletedTodo todoItem ->
        let completedTodo = ActiveTodo ({ Id = id; Text = todoItem.Text })
        let newMap = Map.add id completedTodo model.TodoMap
        { model with TodoMap = newMap } 

let updateTodo (model: Model) (todoItem: TodoItem): Model =
    let todo = Map.find todoItem.Id model.TodoMap
    let newTodo = match todo with
                    | ActiveTodo _todoItem -> 
                        ActiveTodo({ _todoItem with Text = todoItem.Text })
                    | _ -> todo
    let newMap = Map.add todoItem.Id newTodo model.TodoMap
    { model with TodoMap = newMap }

let update (msg:Msg) (model:Model): Model * Cmd<Msg> =
    match msg with
    | CreateTodo -> createTodo model, Cmd.none
    | SwitchTodo id -> switchTodo model id, Cmd.none
    | DeleteTodo id -> { model with TodoMap = Map.remove id model.TodoMap },  Cmd.none
    | UpdateTodo todoItem -> updateTodo model todoItem, Cmd.none
    | UpdateCurrentInput txt -> { model with CurrentInput = txt }, Cmd.none
    | ChangeTab tabState -> { model with TabState = tabState }, Cmd.none

