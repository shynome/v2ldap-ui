module Shared.Msg

open Fable.RemoteData

type ExternalMsg =
  | SetToken of RemoteData<string, string>
