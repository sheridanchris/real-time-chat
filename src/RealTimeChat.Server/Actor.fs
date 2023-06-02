module Actor

open Akka.FSharp
open Shared
open System

type Command =
  | JoinRoom of {| roomId: RoomId; user: User |}
  | LeaveRoom of {| user: User |}
  | SendMessage of {| roomId: RoomId; message: Message |}
  | CreateRoom of {| user: User |}
  | QueryRoomData of {| roomId: RoomId |}

type Event =
  | UserJoinedRoom of {| room: Room; user: User |}
  | UserLeftRoom of {| roomId: RoomId; user: User |}
  | MessageSent of {| roomId: RoomId; message: Message |}
  | RoomCreated of {| room: Room; user: User |}
  | RoomDataQueried of {| room: Room |}

type EventFailure =
  | RoomDoesNotExist of {| roomId: RoomId |}
  | UserIsNotInARoomCurrently
  | UserIsAlreadyInARoom

type ActorResponse =
  | EventsOccurred of Event list
  | CommandFailed of EventFailure

let randomRoomId () =
  let chars = [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '0' .. '9' ]

  [ 1..6 ]
  |> List.map (fun _ -> chars[Random.Shared.Next(0, List.length chars)])
  |> String.Concat
  |> RoomId

let system = System.create "actor-system" (Configuration.defaultConfig ())

let processCommand command rooms =
  match command with
  | JoinRoom command ->
    match rooms |> List.tryFind (fun room -> room.Id = command.roomId) with
    | None -> CommandFailed(RoomDoesNotExist {| roomId = command.roomId |})
    | Some room ->
      EventsOccurred(
        [
          UserJoinedRoom {|
            room = room
            user = {
              command.user with
                  Room = Some room.Id
            }
          |}
          MessageSent {|
            roomId = room.Id
            message =
              Message.SystemMessage {|
                message = $"{command.user.Nickname} has joined."
              |}
          |}
        ]
      )
  | LeaveRoom command ->
    match command.user.Room with
    | None -> CommandFailed UserIsNotInARoomCurrently
    | Some roomId ->
      EventsOccurred(
        [
          UserLeftRoom {|
            roomId = roomId
            user = { command.user with Room = None }
          |}
          MessageSent {|
            roomId = roomId
            message =
              Message.SystemMessage {|
                message = $"{command.user.Nickname} has left."
              |}
          |}
        ]
      )
  | SendMessage command ->
    match rooms |> List.tryFind (fun room -> room.Id = command.roomId) with
    | None -> CommandFailed(RoomDoesNotExist {| roomId = command.roomId |})
    | Some room ->
      EventsOccurred(
        [
          MessageSent {|
            roomId = room.Id
            message = command.message
          |}
        ]
      )
  | CreateRoom command ->
    match command.user.Room with
    | Some _ -> CommandFailed UserIsAlreadyInARoom
    | None ->
      let room = {
        Id = randomRoomId ()
        Users = [ command.user ]
        Messages = []
      }

      EventsOccurred(
        [
          RoomCreated(
            {|
              room = room
              user = {
                command.user with
                    Room = Some room.Id
              }
            |}
          )
        ]
      )
  | QueryRoomData command ->
    match rooms |> List.tryFind (fun room -> room.Id = command.roomId) with
    | None -> CommandFailed(RoomDoesNotExist {| roomId = command.roomId |})
    | Some room -> EventsOccurred([ RoomDataQueried({| room = room |}) ])

let mapRooms roomId mapper rooms =
  rooms |> List.map (fun room -> if room.Id = roomId then mapper room else room)

let processEvent rooms event =
  match event with
  | UserJoinedRoom info ->
    rooms
    |> mapRooms info.room.Id (fun room -> {
      room with
          Users = info.user :: room.Users
    })
  | UserLeftRoom info ->
    rooms
    |> mapRooms info.roomId (fun room -> {
      room with
          Users = room.Users |> List.filter (fun currentUser -> currentUser.Id <> info.user.Id)
    })
  | MessageSent info ->
    rooms
    |> mapRooms info.roomId (fun room -> {
      room with
          Messages = info.message :: room.Messages
    })
  | RoomCreated info -> info.room :: rooms
  | RoomDataQueried _ -> rooms

let chat =
  spawn system "chat-actor" (fun mailbox ->
    let reply message = mailbox.Sender() <! message

    let rec loop rooms =
      actor {
        let! msg = mailbox.Receive()
        let response = processCommand msg rooms

        let rooms =
          match response with
          | CommandFailed _ -> rooms
          | EventsOccurred events -> List.fold processEvent rooms events

        reply response
        return! loop rooms
      }

    loop [])
