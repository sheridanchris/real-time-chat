open Sutil
open Sutil.CoreElements
open type Feliz.length
open Fable.Core.JsInterop

let view () =
  Html.div [
    disposeOnUnmount [ Context.globalContext ]
    Attr.className "h-screen w-screen"
    Bind.el (
      Context.globalContext .> Context.currentView,
      function
      | Context.View.Homepage -> Homepage.view ()
      | Context.View.Chatroom room -> Chatroom.view room
    )
  ]

importSideEffects "./styles.css"
view () |> Program.mount
