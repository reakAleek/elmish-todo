module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Browser = Fable.Import.Browser

// MODEL
type TodoItem = 
    {
        Id : int
        Text : string
    }

type Todo =
    | ActiveTodo of TodoItem
    | CompletedTodo of TodoItem


type Model =
    { 
      TodoMap : Map<int, Todo>
      CurrentInput : string
      CurrentKey : int
    }

type Msg =
| CreateTodo
| CompleteTodo of int
| DeleteTodo of int
| UpdateTodo of TodoItem
| UpdateCurrentInput of string

let init() : Model =
    { CurrentInput = "";
      CurrentKey = 0;
      TodoMap = Map.empty
    }

// UPDATE

let createTodo (model: Model): Model =
    let newTodo = ActiveTodo ({ Id = model.CurrentKey; Text = model.CurrentInput})
    { model with
        CurrentInput = "";
        TodoMap = model.TodoMap.Add(model.CurrentKey, newTodo);
        CurrentKey = model.CurrentKey + 1
    }

let completeTodo (model: Model) (id: int) =
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

let update (msg:Msg) (model:Model) =
    match msg with
    | CreateTodo -> createTodo model
    | CompleteTodo id -> completeTodo model id
    | DeleteTodo id -> {model with TodoMap = Map.remove id model.TodoMap}
    | UpdateTodo todo -> model
    | UpdateCurrentInput txt -> {  model with CurrentInput = txt}


open Fulma
open Fable.Import.React

let renderInputField model dispatch =
    Field.div [Field.HasAddons; Field.CustomClass "main-input-field"]
                                    [
                                        Control.p [Control.IsExpanded][ Input.text
                                        [ 
                                            Input.Placeholder "Add Todo" 
                                            Input.Size IsLarge
                                            Input.Value model.CurrentInput
                                            Input.OnChange (fun ev -> UpdateCurrentInput ev.Value |> dispatch )
                                            Input.Option.Props[OnKeyUp (fun (ev: KeyboardEvent) -> match ev.keyCode with
                                                                                                    | 13. -> dispatch CreateTodo
                                                                                                    | _ -> ())]
                                        ] ]
                                        Control.p []
                                            [
                                                Button.button [Button.Color IsPrimary; Button.Size IsLarge; Button.Props [ OnClick ( fun _ -> CreateTodo |> dispatch ) ]]
                                                    [ 
                                                        str "＋"                                                        
                                                    ]
                                            ]
                                    ]

let renderBaseTodoItem dispatch (icon: ReactElement) (todoItem: TodoItem) =
    div [ Style [ Display "flex"; AlignItems "center"; JustifyContent "space-between"; Flex "1 0 auto" ] ] [
            Button.button [ Button.Color IsWhite; Button.Props[ OnClick ( fun _ -> CompleteTodo todoItem.Id |> dispatch) ] ] [ icon ]
            str todoItem.Text 
            Button.button[ Button.Color IsDanger; Button.Props [ OnClick ( fun _ -> DeleteTodo todoItem.Id |> dispatch ) ]][ str "x"]
        ]                       

let renderActiveTodo dispatch (todoItem: TodoItem) =
    renderBaseTodoItem dispatch (i [ Class "fa fa-circle-thin"] []) todoItem

let renderCompletedToto dispatch (todoItem: TodoItem) =
    renderBaseTodoItem dispatch (i [ Class "fa fa-check-circle-o"] []) todoItem

let renderTodoItem dispatch (todo: Todo) =
    match todo with
    | ActiveTodo todoItem -> todoItem |> renderActiveTodo dispatch
    | CompletedTodo todoItem -> todoItem |> renderCompletedToto dispatch

let view (model:Model) dispatch =   
    Section.section
        []
        [ 
            Container.container [][
                Columns.columns[Columns.IsCentered]
                    [
                        Column.column[Column.Width(Screen.All, Column.Is6)]
                            [
                                Panel.panel[] (Panel.heading[][str "My Todos"] ::  Panel.block [][ renderInputField model dispatch ] :: (Map.toList model.TodoMap |> List.map (fun t -> Panel.block [][ renderTodoItem dispatch (snd t) ])))         
                            ]
                    ]
            ]
        ]
    

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

// App 
Program.mkSimple init update view
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
// |> Program.withConsoleTrace
|> Program.withDebugger
#endif
|> Program.run
