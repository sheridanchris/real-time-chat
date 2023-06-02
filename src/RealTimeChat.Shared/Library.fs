module Shared

open System

type User = { Id: Guid; Nickname: string }

let endpoint = "/ws"
