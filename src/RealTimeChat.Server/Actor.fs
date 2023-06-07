module Actor

open Akka.FSharp
open Shared
open System
open Akka.Actor

type Command =
  | JoinRoom of {| roomId: RoomId; user: User |}
  | LeaveRoom of {| user: User |}
  | CloseConnection of {| user: User |}
  | SendMessage of {| roomId: RoomId; message: Message |}
  | CreateRoom of {| user: User |}
  | QueryRoomData of {| roomId: RoomId |}

type ChatCommand = {
  Command: Command
  SendMessageToClient: RemoteClientMsg -> unit
  SendMessageToRoom: RoomId -> RemoteClientMsg -> unit
}

let randomRoomId () =
  let chars = [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '0' .. '9' ]

  [ 1..6 ]
  |> List.map (fun _ -> chars[Random.Shared.Next(0, List.length chars)])
  |> String.Concat
  |> RoomId

let mapRooms roomId mapper rooms =
  rooms |> List.map (fun room -> if room.Id = roomId then mapper room else room)

let system = System.create "actor-system" (Configuration.defaultConfig ())

let processCommand (userCallback: User -> unit) (chatCommand: ChatCommand) (rooms: Room list) =
  match chatCommand.Command with
  | JoinRoom command ->
    match rooms |> List.tryFind (fun room -> room.Id = command.roomId) with
    | None -> rooms
    | Some room ->
      let user = {
        command.user with
            Room = Some room.Id
      }

      let message =
        Message.SystemMessage {|
          message = $"{user.Nickname} has joined."
        |}

      chatCommand.SendMessageToRoom room.Id (RemoteClientMsg.AddUser user)
      chatCommand.SendMessageToRoom room.Id (RemoteClientMsg.AddMessage message)

      let updatedRoom = {
        room with
            Users = user :: room.Users
            Messages = message :: room.Messages
      }

      chatCommand.SendMessageToClient(RemoteClientMsg.RoomInfo updatedRoom)
      userCallback user
      rooms |> mapRooms room.Id (fun _ -> updatedRoom)
  // TODO: THIS IS ALMOST IDENTICAL TO `LEAVE ROOM`
  | CloseConnection command ->
    match command.user.Room with
    | None -> rooms
    | Some roomId ->
      let message =
        Message.SystemMessage {|
          message = $"{command.user.Nickname} has left."
        |}

      chatCommand.SendMessageToRoom roomId (RemoteClientMsg.RemoveUser command.user.Id)
      chatCommand.SendMessageToRoom roomId (RemoteClientMsg.AddMessage message)

      rooms
      |> mapRooms roomId (fun room -> {
        room with
            Messages = message :: room.Messages
            Users = room.Users |> List.filter (fun roomUser -> roomUser.Id <> command.user.Id)
      })
  | LeaveRoom command ->
    match command.user.Room with
    | None -> rooms
    | Some roomId ->
      let user = { command.user with Room = None }

      let message =
        Message.SystemMessage {|
          message = $"{user.Nickname} has left."
        |}

      chatCommand.SendMessageToRoom roomId (RemoteClientMsg.RemoveUser user.Id)
      chatCommand.SendMessageToRoom roomId (RemoteClientMsg.AddMessage message)
      userCallback user // this is the only difference between this and `CloseConnection`

      rooms
      |> mapRooms roomId (fun room -> {
        room with
            Messages = message :: room.Messages
            Users = room.Users |> List.filter (fun roomUser -> roomUser.Id <> user.Id)
      })
  | SendMessage command ->
    match rooms |> List.tryFind (fun room -> room.Id = command.roomId) with
    | None -> rooms
    | Some room ->
      chatCommand.SendMessageToRoom room.Id (RemoteClientMsg.AddMessage command.message)

      rooms
      |> mapRooms room.Id (fun room -> {
        room with
            Messages = command.message :: room.Messages
      })
  | CreateRoom command ->
    match command.user.Room with
    | Some _ -> rooms
    | None ->
      let roomId = randomRoomId ()
      let user = { command.user with Room = Some roomId }

      let message =
        Message.SystemMessage {|
          message = $"{user.Nickname} has created this room."
        |}

      let room = {
        Id = roomId
        Users = [ user ]
        Messages = [ message ]
      }

      chatCommand.SendMessageToClient(RemoteClientMsg.RoomInfo room)
      userCallback user

      room :: rooms
  | QueryRoomData command ->
    match rooms |> List.tryFind (fun room -> room.Id = command.roomId) with
    | None -> rooms
    | Some room ->
      chatCommand.SendMessageToClient(RemoteClientMsg.RoomInfo room)
      rooms

let chat =
  spawn system "chat-actor" (fun mailbox ->
    let reply message =
      let sender = mailbox.Sender()

      if sender = ActorRefs.NoSender then
        ()
      else
        sender <! message

    let rec loop rooms =
      actor {
        let! msg = mailbox.Receive()
        return! loop (processCommand reply msg rooms)
      }

    loop [])
