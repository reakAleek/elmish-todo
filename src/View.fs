module View

open Types
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React
open Fulma
let renderInputField (model: Model) (dispatch: Msg -> unit) =
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
                            i [ Class "ion ion-md-add"] []                                                        
                        ]
                ]
        ]

let renderBaseTodoItem (dispatch: Msg -> Unit) (icon: ReactElement) (todoItem: TodoItem) (customClass: string) (disabled: bool) =
    div [ Class ("todo-item " + customClass); Style [ Display "flex"; AlignItems "center"; JustifyContent "space-between"; Flex "1 0 auto" ] ] [
            Button.button [ Button.CustomClass "todo-item__check-button"; Button.Color IsWhite; Button.Props[ OnClick ( fun _ -> SwitchTodo todoItem.Id |> dispatch) ] ] [ icon ]
            div [ Style [ FlexGrow "1"; Padding "0 .5rem" ] ] [ 
                Input.text [ 
                    Input.Value todoItem.Text; Input.CustomClass "todo-item__input"
                    Input.OnChange (fun ev -> UpdateTodo { Id = todoItem.Id; Text = ev.Value } |> dispatch )
                    Input.Disabled disabled
                ]
            ]
            Button.button[ Button.CustomClass "todo-item__delete-button"; Button.Color IsDanger; Button.IsOutlined; Button.Props [ OnClick ( fun _ -> DeleteTodo todoItem.Id |> dispatch ) ]]
                [ i [ Class "ion ion-md-close"] [] ]
        ]                       

let renderActiveTodo (dispatch: Msg -> Unit) (todoItem: TodoItem) =
    renderBaseTodoItem dispatch (i [ Class "ion ion-md-radio-button-off"] []) todoItem "todo-item--active" false

let renderCompletedToto (dispatch: Msg -> Unit) (todoItem: TodoItem) =
    renderBaseTodoItem dispatch (i [ Class "ion ion-md-checkmark-circle-outline"] []) todoItem "todo-item--completed" true

let renderTodoItem (dispatch: Msg -> Unit) (todo: Todo) =
    match todo with
    | ActiveTodo todoItem -> todoItem |> renderActiveTodo dispatch
    | CompletedTodo todoItem -> todoItem |> renderCompletedToto dispatch

let renderTabs (model: Model) (dispatch: Msg -> Unit) =
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
        |> List.map (fun todo ->  renderTodoItem dispatch todo));    

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

let view (model:Model) (dispatch: Msg -> Unit) =   
    Section.section []
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
                                    renderTodoList model dispatch |> List.map (fun i -> Panel.block [ Panel.Block.IsActive true ][ i ]);
                                    [ Panel.block [] [ renderFooter model dispatch ] ]                                    
                                ] |> List.concat)
                            ]
                    ]
            ]
        ]
    