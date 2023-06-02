open Sutil
open Sutil.CoreElements

let view () =
  Html.div [
    disposeOnUnmount [ Context.globalContext ]
    Bind.el (
      Context.globalContext .> Context.currentView,
      function
      | Context.View.Homepage -> Homepage.view ()
      | Context.View.Chatroom room -> Chatroom.view room
    )
  ]

view () |> Program.mount
