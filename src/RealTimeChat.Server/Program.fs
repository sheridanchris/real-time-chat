open Saturn
open Giraffe
open Elmish.Bridge

let server =
  Bridge.mkServer Shared.endpoint WebSockets.init WebSockets.update
  |> Bridge.withServerHub WebSockets.hub
  |> Bridge.whenDown WebSockets.ClosedConnection
  |> Bridge.run Giraffe.server

let app =
  application {
    url "http://0.0.0.0:5000"
    use_router server
    app_config Giraffe.useWebSockets
    use_static "public"
  }

run app
