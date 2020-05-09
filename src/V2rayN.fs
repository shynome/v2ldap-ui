module V2rayN

open Thoth.Json
open Fable.Core.JsInterop

type IBase64 =
    abstract encode: string -> string

let Base64: IBase64 = import "Base64" "js-base64"

type V2rayNShare =
    {
      /// version
      v: string
      /// remark
      ps: string option
      /// server address
      add: string
      port: string
      /// VMess_ID
      id: string
      /// alertid
      aid: string option
      /// network
      net: string option
      /// 伪装类型
      ``type``: string option
      /// 伪装的域名
      /// - http host中间逗号(,)隔开
      /// - ws host
      /// - h2 host
      host: string option
      /// ws/h2 stream path
      path: string option
      /// tls
      tls: string option }

type V2ldapShare =
    { url: string
      id: string
      uuid: string }

let toString (wsUrl: string) (id: int) (uuid: string) =

    let uri = System.Uri wsUrl

    let tls = uri.Scheme = "wss"

    let s = uri.Host.Split(":")

    let port =
        if s.Length = 2 then s.[1]
        else if tls then "443"
        else "80"

    let hostname = s.[0]

    let path =
        uri.AbsolutePath + "?user=" + string (id)

    let remark =
        if uri.Fragment = "" then None else Some(uri.Fragment.Substring(1))

    let vs =
        { v = "2"
          path = Some(path)
          ps = remark
          add = hostname
          port = port
          id = uuid
          aid = Some("0")
          net = Some("ws")
          ``type`` = None
          host = None
          tls = if tls then Some("tls") else None }

    let vs = Encode.Auto.toString (0, vs)
    let vs = Base64.encode vs
    let vs = "vmess://" + vs

    vs
