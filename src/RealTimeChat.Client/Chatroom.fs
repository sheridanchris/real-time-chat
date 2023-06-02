module Chatroom

open Sutil
open Sutil.CoreElements
open Shared
open Elmish.Bridge

type Model = { Message: string }

let message model = model.Message

type Msg =
  | SetMessage of string
  | SendMessage
  | Disconnect

let init () = { Message = "" }, Cmd.none

let update msg model =
  match msg with
  | SetMessage message -> { model with Message = message }, Cmd.none
  | SendMessage ->
    { model with Message = "" },
    Cmd.ofEffect (fun _ -> Bridge.Send(RemoteServerMessage.SendMessage {| message = model.Message |}))
  | Disconnect ->
    model,
    Cmd.batch [
      Cmd.ofEffect (fun _ -> Bridge.Send(RemoteServerMessage.Disconnect))
      Cmd.ofEffect (fun _ -> Context.globalDispatch Context.Msg.DisconnectFromChatroom)
    ]

let view room =
  let (RoomId roomId) = room.Id
  let model, dispatch = () |> Store.makeElmish init update ignore

  fragment [
    disposeOnUnmount [ model ]
    Html.h1 $"Room Id: {roomId}"
    for message in room.Messages do
      match message with
      | Message.SystemMessage message -> Html.p message.message
      | Message.ChatMessage message -> Html.p $"{message.senderNickname}: {message.message}"

    Html.input [
      Attr.id "message"
      Attr.placeholder "message"
      Attr.value (model .> message, SetMessage >> dispatch)
    ]
    Html.button [ Attr.text "Send message!"; onClick (fun _ -> dispatch SendMessage) [] ]
    Html.button [ Attr.text "Leave"; onClick (fun _ -> dispatch Disconnect) [] ]
  ]
