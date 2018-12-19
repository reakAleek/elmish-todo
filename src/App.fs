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

let update (msg:Msg) (model:Model) =
    match msg with
    | CreateTodo -> createTodo model
    | CompleteTodo id -> model
    | DeleteTodo id -> model
    | UpdateTodo todo -> model
    | UpdateCurrentInput txt -> {  model with CurrentInput = txt}

    
    
(*     | UpdateDraftForm content ->
        { model with DraftForm = content }
    | CreateDraft ->
        let newDraft = NewDraft model.DraftForm
        { model with
            DraftForm = ""
            Drafts = newDraft::model.Drafts }
    | BumpDraft title ->
        let drafts =
            model.Drafts
            |> List.map (bump title)
        { model with Drafts = drafts }
    | RejectDraft title ->
        let drafts = 
            model.Drafts
            |> List.map (reject title)
        { model with Drafts = drafts }
    | UnBumpDraft title ->
        let drafts = 
            model.Drafts
            |> List.map (unbump title)
        { model with Drafts = drafts } *)

// VIEW (rendered with React)

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

let renderBaseTodoItem (icon: ReactElement) (todoItem: TodoItem) =
    div [ Style [ Display "flex"; AlignItems "center" ] ] [
            Button.button [ Button.Color IsWhite ] [ icon ]
            str todoItem.Text 
        ]                       

let renderActiveTodo (todoItem: TodoItem) =
    renderBaseTodoItem (i [ Class "fa fa-circle-thin"] []) todoItem

let renderCompletedToto (todoItem: TodoItem) =
    renderBaseTodoItem (i [ Class "fa fa-check-circle-o"] []) todoItem

let renderTodoItem (todo: Todo) =
    match todo with
    | ActiveTodo todoItem -> todoItem |> renderActiveTodo
    | CompletedTodo todoItem -> todoItem |> renderCompletedToto

let view (model:Model) dispatch =   
    Section.section
        []
        [ 
            Container.container [][
                Columns.columns[Columns.IsCentered]
                    [
                        Column.column[Column.Width(Screen.All, Column.Is6)]
                            [
                                Panel.panel[] (Panel.heading[][str "My Todos"] ::  Panel.block [][ renderInputField model dispatch ] :: (Map.toList model.TodoMap |> List.map (fun t -> Panel.block [][ renderTodoItem (snd t) ])))         
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
