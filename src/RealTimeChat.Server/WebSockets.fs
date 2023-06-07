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
  | GotActorResponse of User
  | ServerMsg of RemoteServerMessage
  | ClosedConnection

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

let ask msg actor = actor <? msg
let tell msg actor = actor <! msg

let askCmd msg =
  Cmd.OfAsync.perform (ask msg) Actor.chat GotActorResponse

let actorCommand clientDispatch command : Actor.ChatCommand = {
  Command = command
  SendMessageToClient = clientDispatch
  SendMessageToRoom = fun roomId msg -> hub.SendClientIf (sendClientIfInRoom roomId) msg
}

let update clientDispatch serverMsg state =
  printfn $"Processing: %A{serverMsg}"

  match state with
  | NotInitialized ->
    match serverMsg with
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

      | _ -> state, Cmd.none
    | _ -> state, Cmd.none
  | User user ->
    match serverMsg with
    | GotActorResponse state -> User state, Cmd.none
    | ClosedConnection ->
      tell (actorCommand clientDispatch (Actor.Command.CloseConnection {| user = user |})) Actor.chat
      NotInitialized, Cmd.none
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
        state, askCmd (actorCommand clientDispatch (Actor.Command.JoinRoom {| roomId = info.roomId; user = user |}))
      | RemoteServerMessage.SendMessage info ->
        match user.Room with
        | None -> state, Cmd.none
        | Some roomId ->
          state,
          askCmd (
            actorCommand
              clientDispatch
              (Actor.Command.SendMessage {|
                roomId = roomId
                message =
                  Message.ChatMessage {|
                    sender = user
                    message = info.message
                  |}
              |})
          )
      | RemoteServerMessage.CreateRoom ->
        state, askCmd (actorCommand clientDispatch (Actor.Command.CreateRoom {| user = user |}))
      | RemoteServerMessage.Disconnect ->
        state, askCmd (actorCommand clientDispatch (Actor.Command.LeaveRoom {| user = user |}))
