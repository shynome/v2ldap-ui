module Shared.Token

open Fable.Core

type private ILocalStorage =
    abstract getItem: string -> string
    abstract setItem: string * string -> string

[<Global>]
let private localStorage: ILocalStorage = jsNative

let mutable private token =
    let token = localStorage.getItem "token"
    if isNull token then "" else token

let getToken () = token

let setToken newToken =
    token <- newToken
    localStorage.setItem ("token", newToken)
