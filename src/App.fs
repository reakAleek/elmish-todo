module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Browser = Fable.Import.Browser

// MODEL

type TabState =
| AllTab
| ActiveTab
| CompletedTab

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
      TabState: TabState
    }

type Msg =
| CreateTodo
| CompleteTodo of int
| DeleteTodo of int
| UpdateTodo of TodoItem
| UpdateCurrentInput of string
| ChangeTab of TabState

let init() : Model =
    { CurrentInput = "";
      CurrentKey = 0;
      TodoMap = Map.empty;
      TabState = AllTab
    }

// UPDATE

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

let updateTodo (model: Model) (todoItem: TodoItem): Model =
    let todo = Map.find todoItem.Id model.TodoMap
    let newTodo = match todo with
                    | ActiveTodo _todoItem -> ActiveTodo({ _todoItem with Text = todoItem.Text })
                    | _ -> todo
    let newMap = Map.add todoItem.Id newTodo model.TodoMap
    { model with TodoMap = newMap }

let update (msg:Msg) (model:Model) =
    match msg with
    | CreateTodo -> createTodo model
    | CompleteTodo id -> completeTodo model id
    | DeleteTodo id -> {model with TodoMap = Map.remove id model.TodoMap}
    | UpdateTodo todoItem -> updateTodo model todoItem
    | UpdateCurrentInput txt -> {  model with CurrentInput = txt}
    | ChangeTab tabState -> { model with TabState = tabState }    


open Fulma
open Fable.Import.React

let renderInputField model dispatch =
    Field.div [Field.HasAddons; Field.CustomClass "main-input-field"]
                                    [
                                        Control.p [Control.IsExpanded][ Input.text
                                        [ 
                                            Input.Placeholder "Add todo..." 
                                            Input.Size IsMedium
                                            Input.Value model.CurrentInput
                                            Input.OnChange (fun ev -> UpdateCurrentInput ev.Value |> dispatch )
                                            Input.Option.Props[OnKeyUp (fun (ev: KeyboardEvent) -> match ev.keyCode with
                                                                                                    | 13. -> dispatch CreateTodo
                                                                                                    | _ -> ())]
                                        ] ]
                                        Control.p []
                                            [
                                                Button.button [Button.Color IsPrimary; Button.IsOutlined; Button.Size IsMedium; Button.Props [ OnClick ( fun _ -> CreateTodo |> dispatch ) ]]
                                                    [ 
                                                        i [ Class "fa fa-plus"] []                                                        
                                                    ]
                                            ]
                                    ]

let renderBaseTodoItem dispatch (icon: ReactElement) (todoItem: TodoItem) (customClass: string) (disabled: bool) =
    div [ Class ("todo-item " + customClass); Style [ Display "flex"; AlignItems "center"; JustifyContent "space-between"; Flex "1 0 auto" ] ] [
            Button.button [ Button.CustomClass "todo-item__check-button"; Button.Color IsWhite; Button.Props[ OnClick ( fun _ -> CompleteTodo todoItem.Id |> dispatch) ] ] [ icon ]
            div [ Style [ FlexGrow "1"; Padding "0 .5rem" ] ] [ 
                Input.text [ 
                    Input.Value todoItem.Text; Input.CustomClass "todo-item-input"
                    Input.OnChange (fun ev -> UpdateTodo { Id = todoItem.Id; Text = ev.Value } |> dispatch )
                    Input.Disabled disabled
                ]
            ]
            Button.button[ Button.CustomClass "todo-item__delete-button"; Button.Color IsDanger; Button.IsOutlined; Button.Props [ OnClick ( fun _ -> DeleteTodo todoItem.Id |> dispatch ) ]][ i [ Class "fa fa-times"] [] ]
        ]                       

let renderActiveTodo dispatch (todoItem: TodoItem) =
    renderBaseTodoItem dispatch (i [ Class "fa fa-circle-o"] []) todoItem "todo-item--active" false

let renderCompletedToto dispatch (todoItem: TodoItem) =
    renderBaseTodoItem dispatch (i [ Class "fa fa-check-circle-o"] []) todoItem "todo-item--completed" true

let renderTodoItem dispatch (todo: Todo) =
    match todo with
    | ActiveTodo todoItem -> todoItem |> renderActiveTodo dispatch
    | CompletedTodo todoItem -> todoItem |> renderCompletedToto dispatch

let renderTabs (model: Model) dispatch =
    let (a,b,c) = match model.TabState with
                    | AllTab -> true, false, false
                    | ActiveTab -> false, true, false
                    | CompletedTab -> false, false, true
    Panel.tabs [] [
        Panel.tab [ Panel.Tab.IsActive a ] [ div [ OnClick (fun _ -> ChangeTab AllTab |> dispatch)] [ str "All" ] ]
        Panel.tab [ Panel.Tab.IsActive b ] [ div [ OnClick (fun _ -> ChangeTab ActiveTab |> dispatch)] [ str "Active" ]]
        Panel.tab [ Panel.Tab.IsActive c ] [ div [ OnClick (fun _ -> ChangeTab CompletedTab |> dispatch)] [ str "Complete" ] ]
    ]

let filterByTabState (tabState: TabState) (list: List<Todo>): List<Todo> =
    match tabState with
    | AllTab -> list
    | ActiveTab -> list |> List.choose (fun todo -> 
        match todo with
        | ActiveTodo _ -> Some todo
        | _ -> None
        )
    | CompletedTab -> list |> List.choose (fun todo -> 
        match todo with
        | CompletedTodo _ -> Some todo
        | _ -> None
        )


let renderTodoList (model: Model) (dispatch: Msg -> unit): List<ReactElement> =
    ((Map.toList model.TodoMap)
        |> List.rev
        |> List.map snd
        |> filterByTabState model.TabState
        |> List.map (fun todo -> Panel.block [ Panel.Block.IsActive true ][ renderTodoItem dispatch todo ]));    

let getActiveCount (todoList: List<Todo>): int =
    todoList |> List.choose (fun todo -> 
        match todo with
        | ActiveTodo _ -> Some todo
        | _ -> None
    ) |> List.length

let renderFooter (model: Model) (dispatch: Msg -> unit): ReactElement =
    let count = (model.TodoMap |> Map.toList |> List.map snd |> getActiveCount)
    Tag.tag [ Tag.Size IsSmall ] [
        (match count with
        | 0 -> str "No todos"
        | _ -> str ((string count) + " todos left"))
    ]

let view (model:Model) dispatch =   
    Section.section
        []
        [ 
            Container.container [][
                Columns.columns[Columns.IsCentered]
                    [
                        Column.column[Column.Width(Screen.All, Column.Is6)]
                            [
                                Panel.panel[] ([
                                    [ (Panel.heading[] [str "My todos"]) ];
                                    [ renderTabs model dispatch ];
                                    [ Panel.block [][ renderInputField model dispatch ]; ]
                                    renderTodoList model dispatch;
                                    [ Panel.block [] [ renderFooter model dispatch ] ]                                    
                                ] |> List.concat)
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
