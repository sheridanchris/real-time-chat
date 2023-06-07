module Chatroom

open Sutil
open Sutil.CoreElements
open Shared
open Elmish.Bridge
open type Feliz.length
open type Feliz.borderStyle

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

let cssColorFromUser user =
  match user.Color with
  | Red -> "text-red-500"
  | Green -> "text-green-500"
  | Blue -> "text-blue-500"
  | Orange -> "text-orange-500"
  | Yellow -> "text-yellow-500"
  | Purple -> "text-purple-500"
  | Pink -> "text-pink-500"

let view room =
  let (RoomId roomId) = room.Id
  let model, dispatch = () |> Store.makeElmish init update ignore

  Html.div [
    disposeOnUnmount [ model ]
    Attr.className "flex flex-row w-full h-full divide-x divide-solid divide-black"
    headTitle $"Room: {roomId}"

    // chatbox
    Html.div [
      Attr.id "chatbox"
      Attr.className "flex flex-col w-[90%] h-[full]"

      // chat
      Html.div [
        Attr.className "flex flex-col w-full h-[95%] gap-y-1 overflow-scroll span-y"
        for message in room.Messages |> List.rev do
          match message with
          | Message.SystemMessage sysMsg -> Html.p [ Attr.className "text-gray-600"; Attr.text sysMsg.message ]
          | Message.ChatMessage chatMsg ->
            Html.span [
              Attr.className "flex flex-row gap-x-2"

              Html.p [
                Attr.className (cssColorFromUser chatMsg.sender)
                Attr.text chatMsg.sender.Nickname
              ]

              text chatMsg.message
            ]
      ]

      // message input
      Html.div [
        Attr.id "message-input"
        Attr.className "h-[5%] w-full"
        Html.textarea [
          // TODO: make the input taller if there is overflow
          Attr.className "h-full w-full border border-black pl-1"
          Attr.placeholder "message"
          Attr.value (model .> message, SetMessage >> dispatch)
          onKeyDown (fun event -> if event.key = "Enter" then dispatch SendMessage else ()) []
        ]
      ]
    ]

    // users list
    Html.div [
      Attr.id "user-list"
      Attr.className "flex w-[10%] h-full justify-center"

      Html.ul [
        for user in room.Users |> List.rev do
          Html.li [ Attr.className (cssColorFromUser user); Attr.text user.Nickname ]
      ]
    ]
  ]
