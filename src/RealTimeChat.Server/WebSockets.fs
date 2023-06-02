module WebSockets

open System
open Akka.FSharp
open Elmish
open Elmish.Bridge
open Shared

type State =
  | NotInitialized
  | User of User

type ServerMsg =
  | ServerMsg of RemoteServerMessage
  | GotActorResponse of Actor.ActorResponse
  | ClosedConnection

let askCmd message actor =
  let ask message actor = actor <? message
  Cmd.OfAsync.perform (ask message) actor GotActorResponse

let hub = ServerHub<State, ServerMsg, RemoteClientMsg>()

let init clientDispatch =
  fun () ->
    clientDispatch RemoteClientMsg.ConnectionEstablished
    NotInitialized, Cmd.none

let pickRandomColor () =
  let options = [ Red; Green; Blue; Orange; Yellow; Purple; Pink ]
  options[Random.Shared.Next(0, List.length options)]

let sendClientIfInRoom room client =
  match client with
  | User user when user.Room = Some room -> true
  | _ -> false

let update clientDispatch serverMsg state =
  printfn $"Processing: %A{serverMsg}"

  match state with
  | NotInitialized ->
    match serverMsg with
    | ClosedConnection -> NotInitialized, Cmd.none
    | GotActorResponse _ -> NotInitialized, Cmd.none
    | ServerMsg msg ->
      match msg with
      | RemoteServerMessage.InitializeUser info ->
        let user = {
          Id = UserId(Guid.NewGuid())
          Nickname = info.nickname
          Room = None
          Color = pickRandomColor ()
        }

        clientDispatch (RemoteClientMsg.CurrentUserInitialized user)
        User user, Cmd.none
      | _ -> NotInitialized, Cmd.none
  | User user ->
    match serverMsg with
    | ClosedConnection ->
      match user.Room with
      | None -> NotInitialized, Cmd.none
      // I shouldn't be expecting a reply here... so `asking` shouldn't matter.
      | Some _ -> NotInitialized, askCmd (Actor.Command.LeaveRoom {| user = user |}) Actor.chat
    | ServerMsg msg ->
      match msg with
      // Can't initialize if you're already intialized
      | RemoteServerMessage.InitializeUser info -> state, Cmd.none
      // Can only set nickname when you're not in a room.
      | RemoteServerMessage.UpdateNickname info ->
        match user.Room with
        | Some _ -> state, Cmd.none
        | None ->
          let user = { user with Nickname = info.nickname }
          clientDispatch (RemoteClientMsg.CurrentUserInfoUpdated user)
          User user, Cmd.none
      | RemoteServerMessage.JoinRoom info ->
        state, askCmd (Actor.Command.JoinRoom {| roomId = info.roomId; user = user |}) Actor.chat
      | RemoteServerMessage.SendMessage info ->
        match user.Room with
        | None -> state, Cmd.none
        | Some roomId ->
          state,
          askCmd
            (Actor.Command.SendMessage {|
              roomId = roomId
              message =
                Message.ChatMessage {|
                  senderId = user.Id
                  senderNickname = user.Nickname
                  message = info.message
                |}
            |})
            Actor.chat
      | RemoteServerMessage.CreateRoom -> state, askCmd (Actor.Command.CreateRoom {| user = user |}) Actor.chat
      | RemoteServerMessage.Disconnect -> state, askCmd (Actor.Command.LeaveRoom {| user = user |}) Actor.chat
    | GotActorResponse response ->
      match response with
      | Actor.ActorResponse.CommandFailed _ -> state, Cmd.none
      | Actor.ActorResponse.EventsOccurred events ->
        let state =
          List.fold
            (fun user event ->
              match event with
              | Actor.Event.RoomCreated info ->
                clientDispatch (RemoteClientMsg.RoomInfo info.room)
                User info.user
              | Actor.Event.RoomDataQueried info ->
                clientDispatch (RemoteClientMsg.RoomInfo info.room)
                user
              | Actor.Event.MessageSent info ->
                hub.SendClientIf (sendClientIfInRoom info.roomId) (RemoteClientMsg.AddMessage info.message)
                user
              | Actor.Event.UserJoinedRoom info ->
                clientDispatch (RemoteClientMsg.RoomInfo info.room)
                hub.SendClientIf (sendClientIfInRoom info.room.Id) (RemoteClientMsg.AddUser info.user)
                User info.user
              | Actor.Event.UserLeftRoom info ->
                hub.SendClientIf (sendClientIfInRoom info.roomId) (RemoteClientMsg.RemoveUser info.user)
                User info.user)
            state
            events

        state, Cmd.none
