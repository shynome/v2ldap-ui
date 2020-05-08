module Shared.Api

open Fable.SimpleHttp
open Fable.RemoteData
open Thoth.Json
open Types
open Shared.Token

let private baseUrl =
#if DEBUG
    "http://127.0.0.1:7070/api/user"
#else
    "/api/user"
#endif

let private makeRequest url =
    let token = getToken ()
    let token = "Bearer " + token
    Http.request (baseUrl + url)
    |> Http.header (Header("Authorization", token))

let getUsers () =
    async {
        let req = makeRequest "/list"
        let! res = req |> Http.send

        let v =
            match res.statusCode with
            | 200 ->
                let result =
                    Decode.Auto.fromString<Response<User list>> (res.responseText)

                match result with
                | Ok v ->
                    match v.data with
                    | Some users -> Success users
                    | None -> Failure v.error.Value
                | Error e -> Failure e
            | _ -> Failure res.responseText

        return v
    }

let getUser email =
    async {
        let req = makeRequest ("/get?user=" + email)
        let! res = req |> Http.send

        let v =
            match res.statusCode with
            | 200 ->
                let result =
                    Decode.Auto.fromString<Response<User>> (res.responseText)

                match result with
                | Ok v ->
                    match v.data with
                    | Some user -> Success user
                    | None -> Failure v.error.Value
                | Error e -> Failure e
            | _ -> Failure res.responseText

        return v
    }

type UpdateField<'a> = { run: bool; ``val``: 'a }
type Updates = {
    disable: UpdateField<bool> option
    uuid: UpdateField<string> option
    remark: UpdateField<string> option
}
type UpdateData = { id: int; update: Updates }

let updateUser id updates =
    async {
        let data = { id = id; update = updates }
        let data = Encode.Auto.toString (0, data)

        let! res =
            makeRequest ("/update")
            |> Http.method POST
            |> Http.header (Headers.contentType "application/json")
            |> Http.content (BodyContent.Text data)
            |> Http.send

        let v =
            match res.statusCode with
            | 200 ->
                let result =
                    Decode.Auto.fromString<Response<User>> (res.responseText)

                match result with
                | Ok v ->
                    match v.data with
                    | Some user -> Success user
                    | None -> Failure v.error.Value
                | Error e -> Failure e
            | _ -> Failure res.responseText

        return v
    }

type AuthData = { secret_key: string }

let login secretKey =
    async {
        let data = { secret_key = secretKey }
        let data = Encode.Auto.toString (0, data)
        let! res =
            makeRequest ("/login")
            |> Http.method POST
            |> Http.header (Headers.contentType "application/json")
            |> Http.content (BodyContent.Text data)
            |> Http.send

        let v =
            match res.statusCode with
            | 200 ->
                let result =
                    Decode.Auto.fromString<Response<string>> (res.responseText)

                match result with
                | Ok v ->
                    System.Console.WriteLine v.data.Value
                    Success v.data.Value
                | Error e -> Failure e
            | _ -> Failure res.responseText

        return v
    }

let verify () =
    async {
        let! res = makeRequest ("/whoami") |> Http.send

        let v =
            match res.statusCode with
            | 200 ->
                let result =
                    Decode.Auto.fromString<Response<string>> (res.responseText)

                match result with
                | Ok v -> Success ""
                | Error e -> Failure e
            | _ -> Failure res.responseText

        return v
    }
