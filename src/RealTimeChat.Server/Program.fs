open Saturn
open Giraffe
open Elmish.Bridge
open Shared

type State =
  | Unknown
  | User of User

let init _ _ = Unknown, Elmish.Cmd.none

let update dispatch msg state = state, Elmish.Cmd.none

let server =
  Bridge.mkServer Shared.endpoint init update |> Bridge.run Giraffe.server

let webApp = choose [ server ]

let app =
  application {
    url "http://0.0.0.0:5000"
    use_router webApp
    app_config Giraffe.useWebSockets
    use_static "public"
  }

run app
