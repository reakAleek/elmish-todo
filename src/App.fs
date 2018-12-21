module App

open Elmish
open Elmish.React
open Types
open Update
open View
// MODEL

let init() : Model * Cmd<Msg> =
    {
        CurrentInput = ""
        CurrentKey = 0
        TodoMap = Map.empty
        TabState = AllTab
    }, Cmd.none

// UPDATE

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

// App 
Program.mkProgram init update view
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
// |> Program.withConsoleTrace
|> Program.withDebugger
#endif
|> Program.run
