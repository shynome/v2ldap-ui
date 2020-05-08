module Login

open Elmish
open Feliz
open Feliz.MaterialUI
open Fable.RemoteData
open Shared.Api
open Shared.Msg

type State = {
    Secret: string
    VerifyResult: RemoteData<string, string>
}

type Msg =
    | Login
    | SetSecret of string
    | SetVerifyResult of RemoteData<string, string>

let init () = { Secret = ""; VerifyResult = NotAsked }, Cmd.none

let update msg (state:State) =
    match msg with
    | Login ->
        state, Cmd.OfAsync.perform login state.Secret SetVerifyResult, Cmd.none
    | SetSecret secret ->
        {state with Secret = secret}, Cmd.none, Cmd.none
    | SetVerifyResult result ->
        let cmd =
            match result with
            | Success -> Cmd.ofMsg (ExternalMsg.SetToken result)
            | _ -> Cmd.none
        {state with VerifyResult = result}, Cmd.none, cmd

let renderError (err:string) =
    Mui.alert [
        alert.severity.error
        prop.children [
            Mui.alertTitle [
                prop.text "登录出错"
            ]
            Html.text err
        ]
    ]

let renderCard state dispatch =
    let loading =
        match state.VerifyResult with
        | Loading -> true
        | _ -> false
    Mui.card [
        Mui.cardHeader [
            cardHeader.title "登录"
        ]
        match state.VerifyResult with
        | Failure e ->
            Mui.cardContent [
                renderError e
            ]
        | _ -> Html.none
        Mui.cardContent [
            Mui.textField [
                textField.label "密钥"
                textField.fullWidth true
                textField.variant.outlined
                textField.type' "password"
                textField.required true
                textField.disabled loading
                textField.onChange (SetSecret >> dispatch)
            ]
        ]
        Mui.cardActions [
            Mui.button [
                button.color.primary
                button.variant.contained
                button.type'.submit
                button.disabled loading
                prop.text "登录"
            ]
        ]
    ]

let render (state:State) dispatch =
    Html.form [
        prop.onSubmit (fun e-> e.preventDefault(); Login |> dispatch)
        prop.children [
            renderCard state dispatch
        ]
    ]
