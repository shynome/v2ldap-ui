module App

open Feliz
open Elmish
open Feliz.MaterialUI
open Fable.RemoteData
open Shared.Msg
open Shared.Token
open Shared.Api

type State = {
    Token: RemoteData<string, string>
    Login: Login.State
    Users: Users.State
}

type Msg =
    | ExternalMsg of ExternalMsg
    | Login of Login.Msg
    | Users of Users.Msg
    | VerifyToken

let init() =
    let (ls,lc) = Login.init()
    let (us,uc) = Users.init()
    let token = getToken()
    let cmd = if token = "" then Cmd.none else Cmd.ofMsg VerifyToken
    {
        Token = if token = "" then Failure "" else Loading
        Login = ls
        Users = us
    }, Cmd.batch [
        Cmd.map Login lc
        Cmd.map Users uc
        cmd
    ]

let handleExternalMsg msg state =
    match msg with
    | SetToken token ->
        let cmd =
            match token with
            | Success token ->
                if token <> "" then setToken token |> ignore
                Cmd.ofMsg (Users Users.Msg.Reload)
            | _ -> Cmd.none
        {state with Token = token}, cmd

let update (msg: Msg) (state: State) =
    match msg with
    | Login msg ->
        let ls, cmd, externalCmd = Login.update msg state.Login
        let cmd = Cmd.batch [
            Cmd.map Login cmd
            Cmd.map ExternalMsg externalCmd
        ]
        {state with Login = ls}, cmd
    | Users msg ->
        let us, cmd = Users.update msg state.Users
        let cmd = Cmd.map Users cmd
        { state with Users = us }, cmd
    | VerifyToken ->
        state, Cmd.OfAsync.perform verify () (SetToken >> ExternalMsg)
    | ExternalMsg msg ->
        handleExternalMsg msg state

let render (state: State) (dispatch: Msg -> unit) =
    let showLoginDialog =
        match state.Token with
        | Failure -> true
        | _ -> false
    Mui.container [
        match state.Token with
        | Loading -> Mui.linearProgress []
        | _ -> Html.none
        Mui.dialog [
            dialog.open' showLoginDialog
            dialog.fullWidth true
            dialog.maxWidth.sm
            prop.children [
                Login.render state.Login (Login >> dispatch)
            ]
        ]
        Users.render state.Users (Users >> dispatch)
    ]