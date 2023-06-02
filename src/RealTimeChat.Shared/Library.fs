module Shared

open System

type UserId = UserId of Guid
type RoomId = RoomId of string

type Color =
  | Red
  | Green
  | Blue
  | Orange
  | Yellow
  | Purple
  | Pink

type User = {
  Id: UserId
  Nickname: string
  Room: RoomId option
  Color: Color
}

[<RequireQualifiedAccess>]
type Message =
  | SystemMessage of {| message: string |}
  | ChatMessage of
    {|
      senderId: UserId
      senderNickname: string
      message: string
    |}

type Room = {
  Id: RoomId
  Users: User list
  Messages: Message list
}

[<RequireQualifiedAccess>]
type RemoteClientMsg =
  | RoomInfo of Room
  | AddMessage of Message
  | AddUser of User
  | RemoveUser of User

[<RequireQualifiedAccess>]
type RemoteServerMessage =
  | SetNickname of {| nickname: string |}
  | CreateRoom
  | JoinRoom of {| roomId: RoomId |}
  | SendMessage of {| message: string |}

let endpoint = "/ws"
