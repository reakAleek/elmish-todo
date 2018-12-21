module Types

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
| SwitchTodo of int
| DeleteTodo of int
| UpdateTodo of TodoItem
| UpdateCurrentInput of string
| ChangeTab of TabState