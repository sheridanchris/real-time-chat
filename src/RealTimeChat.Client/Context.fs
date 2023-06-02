module Context

open Sutil
open Shared
open Elmish.Bridge

type Status =
  | NotConnectedToServer
  | NotInitialized
  | Initialized of User

type View =
  | Homepage
  | Chatroom of Room

type Model = { CurrentView: View; Status: Status }

let currentView model = model.CurrentView
let status model = model.Status

type Msg =
  | ConnectionLost
  | GotClientMsg of RemoteClientMsg
  | DisconnectFromChatroom

let init () =
  {
    CurrentView = Homepage
    Status = NotConnectedToServer
  },
  Cmd.none

let update msg model =
  match msg with
  | ConnectionLost ->
    {
      model with
          Status = NotConnectedToServer
          CurrentView = Homepage
    },
    Cmd.none
  | GotClientMsg clientMsg ->
    match clientMsg with
    | RemoteClientMsg.ConnectionEstablished -> { model with Status = NotInitialized }, Cmd.none
    | RemoteClientMsg.CurrentUserInitialized user
    | RemoteClientMsg.CurrentUserInfoUpdated user -> { model with Status = Initialized user }, Cmd.none
    | RemoteClientMsg.RoomInfo roomInfo ->
      {
        model with
            CurrentView = Chatroom roomInfo
      },
      Cmd.none
    | RemoteClientMsg.AddMessage message ->
      match model.CurrentView with
      | Homepage -> model, Cmd.none
      | Chatroom room ->
        {
          model with
              CurrentView =
                Chatroom {
                  room with
                      Messages = message :: room.Messages
                }
        },
        Cmd.none
    | RemoteClientMsg.AddUser user ->
      match model.CurrentView with
      | Homepage -> model, Cmd.none
      | Chatroom room ->
        {
          model with
              CurrentView = Chatroom { room with Users = user :: room.Users }
        },
        Cmd.none
    | RemoteClientMsg.RemoveUser user ->
      match model.CurrentView with
      | Homepage -> model, Cmd.none
      | Chatroom room ->
        {
          model with
              CurrentView =
                Chatroom {
                  room with
                      Users = room.Users |> List.filter (fun cUser -> user.Id <> cUser.Id)
                }
        },
        Cmd.none
  | DisconnectFromChatroom -> { model with CurrentView = Homepage }, Cmd.none

let globalContext, globalDispatch = () |> Store.makeElmish init update ignore

Bridge.endpoint Shared.endpoint
|> Bridge.withMapping GotClientMsg
|> Bridge.withWhenDown ConnectionLost
|> Bridge.onCustomDispatcher (fun msg -> globalDispatch msg)
