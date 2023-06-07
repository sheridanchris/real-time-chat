module Homepage

open Sutil
open Sutil.CoreElements
open Elmish.Bridge
open Shared

type Model = { Nickname: string; RoomCode: string }

let nickname model = model.Nickname
let roomCode model = model.RoomCode

type Msg =
  | SetNickname of string
  | SetRoomCode of string
  | CreateRoom
  | JoinRoom

let init () =
  let globalStore = Store.get Context.globalContext

  let nickname =
    match globalStore.Status with
    | Context.Status.NotConnectedToServer
    | Context.Status.NotInitialized -> ""
    | Context.Status.Initialized user -> user.Nickname

  { Nickname = nickname; RoomCode = "" }, Cmd.none

let update msg model =
  match msg with
  | SetNickname nickname -> { model with Model.Nickname = nickname }, Cmd.none
  | SetRoomCode code -> { model with RoomCode = code }, Cmd.none
  | CreateRoom ->
    let globalStore = Store.get Context.globalContext

    match globalStore.Status with
    | Context.Status.NotConnectedToServer -> model, Cmd.none
    | Context.Status.Initialized _ -> model, Cmd.ofEffect (fun _ -> Bridge.Send(RemoteServerMessage.CreateRoom))
    | Context.Status.NotInitialized ->
      model,
      Cmd.batch [
        Cmd.ofEffect (fun _ -> Bridge.Send(RemoteServerMessage.InitializeUser {| nickname = model.Nickname |}))
        Cmd.ofEffect (fun _ -> Bridge.Send(RemoteServerMessage.CreateRoom))
      ]
  | JoinRoom ->
    let roomId = RoomId model.RoomCode
    let globalStore = Store.get Context.globalContext

    match globalStore.Status with
    | Context.Status.NotConnectedToServer -> model, Cmd.none
    | Context.Status.Initialized _ ->
      model, Cmd.ofEffect (fun _ -> Bridge.Send(RemoteServerMessage.JoinRoom {| roomId = roomId |}))
    | Context.Status.NotInitialized ->
      model,
      Cmd.batch [
        Cmd.ofEffect (fun _ -> Bridge.Send(RemoteServerMessage.InitializeUser {| nickname = model.Nickname |}))
        Cmd.ofEffect (fun _ -> Bridge.Send(RemoteServerMessage.JoinRoom {| roomId = roomId |}))
      ]

let input elements =
  Html.input [
    Attr.className "border border-gray-900 rounded-sm px-1.5 py-1.5 placeholder-gray-600"
    yield! elements
  ]

let button elements =
  Html.button [
    Attr.className "border rounded-sm border-green-600 bg-green-600 text-white px-1.5 py-1.5"
    yield! elements
  ]

let view () =
  let model, dispatch = () |> Store.makeElmish init update ignore

  Html.div [
    disposeOnUnmount [ model ]
    Attr.className "flex flex-col h-screen w-screen justify-center items-center"

    Html.div [
      Attr.className "flex flex-col w-1/4 gap-y-2.5"

      input [
        Attr.id "nickname"
        Attr.placeholder "nickname"
        Attr.value (model .> nickname, SetNickname >> dispatch)
      ]
      input [
        Attr.id "room-code"
        Attr.className "border border-gray-900 rounded-sm px-1.5 py-1.5 placeholder-gray-600"
        Attr.placeholder "room code"
        Attr.value (model .> roomCode, SetRoomCode >> dispatch)
      ]
      Html.div [
        Attr.className "flex flex-row gap-x-2.5 w-full"
        button [
          Attr.className "w-2/4"
          Attr.text "Create room"
          onClick (fun _ -> dispatch CreateRoom) []
        ]
        button [
          Attr.className "w-2/4"
          Attr.text "Join room"
          onClick (fun _ -> dispatch JoinRoom) []
        ]
      ]
    ]
  ]
