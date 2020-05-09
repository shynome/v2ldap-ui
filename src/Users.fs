module Users

open Feliz
open Elmish
open Feliz.MaterialUI
open Fable.MaterialUI.Icons
open Shared.Types
open Shared.Api
open Fable.RemoteData
open Components

type State = {
    Users: RemoteData<string, list<User>>
    QRCode: User option
    LinkConfig: RemoteData<string,LinkConfig>
}

type Msg =
    | SetUsers of RemoteData<string, list<User>>
    | SetUser of User
    | SetQRCode of User option
    | GetLinkConfig
    | SetLinkConfig of RemoteData<string,LinkConfig>
    | Reload

let init() =
    { Users = NotAsked; QRCode = None; LinkConfig = NotAsked }, Cmd.none

let update (msg: Msg) (state: State) =
    match msg with
    | SetUsers users -> { state with Users = users }, Cmd.none
    | SetUser user ->
        match state.Users with
        | Success users ->
            let users = users |> List.map (fun u -> if u.ID = user.ID then user else u )
            { state with Users = Success users}, Cmd.none
        | _ -> state, Cmd.none
    | SetQRCode u ->
        { state with QRCode = u }, Cmd.none
    | Reload ->
        let loadLinkConfigCmd =
            match state.LinkConfig with
            | NotAsked | Failure -> Cmd.ofMsg GetLinkConfig
            | _ -> Cmd.none
        state, Cmd.batch [
            Cmd.OfAsync.perform getUsers () SetUsers
            loadLinkConfigCmd
        ]
    | GetLinkConfig ->
        state, Cmd.OfAsync.perform getLinkConfig () SetLinkConfig
    | SetLinkConfig config ->
        {state with LinkConfig = config}, Cmd.none


type DisableUserProps = {
    User: User
    Dispatch: Msg->unit
}
let renderDisableBtn = React.functionComponent(fun (props: DisableUserProps) ->
    let (inProgress, setInProgress) = React.useState(false)
    let toggle =
        React.useCallback(
            fun (disableStatus:bool)->
                async {
                    setInProgress true
                    let updates = {
                        disable = Some({ run = true; ``val`` = disableStatus; })
                        uuid = None
                        remark = None
                    }
                    let! res = updateUser props.User.ID updates
                    match res with
                    | Success user -> props.Dispatch (SetUser user)
                    | _ -> ()
                    setInProgress false
                    ()
                } |> Async.StartImmediate
                ()
            , [|props.User.ID|]
        )
    let title =
        match (inProgress, props.User.disabled) with
        | (true, _) -> "正在修改中"
        | (_, true) -> "点击激活"
        | (_, false) -> "点击禁用"
    Mui.tooltip [
        tooltip.title title
        tooltip.children (
            Html.span [
                Mui.checkbox [
                    checkbox.checked' (not props.User.disabled)
                    checkbox.color.primary
                    checkbox.disabled inProgress
                    prop.onClick (fun _->toggle (not props.User.disabled))
                ]
            ]
        )
    ]
)

type RemarkUserProps = {
    User: User
    Dispatch: Msg->unit
}
let renderRemark = React.functionComponent(fun (props: RemarkUserProps) ->
    let (inProgress, setInProgress) = React.useState(false)
    let (isEdit,setEdit) = React.useState(false)
    let (val', setVal) = React.useState(props.User.remark)
    let save = fun ()->
        async {
            if val' = "" then return ()
            setInProgress true
            let updates = {
                disable = None
                uuid = None
                remark = Some({ run = true; ``val`` = val'; })
            }
            let! res = updateUser props.User.ID updates
            match res with
            | Success user -> props.Dispatch (SetUser user)
            | _ -> ()
            setInProgress false
            setEdit false
            ()
        } |> Async.StartImmediate
        ()
    match (isEdit, val') with
    | (true, val') ->
        Html.form [
            prop.onSubmit (fun e->e.preventDefault();save())
            prop.children [
                Mui.input [
                    input.value val'
                    input.onChange setVal
                    input.disabled inProgress
                    input.autoFocus true
                    input.endAdornment (
                        Mui.buttonGroup [
                            prop.style [
                                style.wordBreak.keepAll
                            ]
                            buttonGroup.variant.text
                            buttonGroup.disabled inProgress
                            prop.children [
                                Mui.button [
                                    prop.text "保存"
                                    button.type'.submit
                                ]
                                Mui.button [
                                    prop.text "取消"
                                    prop.onClick (fun _->
                                        setEdit false
                                        setVal props.User.remark
                                    )
                                ]
                            ]
                        ]
                    )
                ]
            ]
        ]
    | (_,"") ->
        Mui.tooltip [
            tooltip.title "点击添加备注"
            tooltip.children (
                Mui.button [
                    prop.text "添加备注"
                    prop.onClick (fun _->setEdit(true))
                ]
            )
        ]
    | (false, val') ->
        Mui.tooltip [
            tooltip.title "点击编辑"
            tooltip.children (
                Mui.button [
                    prop.text val'
                    prop.style [
                        style.textTransform.none
                    ]
                    prop.onClick (fun _->setEdit true)
                ]
            )
        ]

)

type QRDialogProps = {
    State: State
    Dispatch: Msg->unit
}
let renderQRDialog = React.functionComponent(fun (props: QRDialogProps) ->
    let (vs, setVs) = React.useState("")
    React.useEffect((fun _->
        match props.State.QRCode with
        | None -> ()
        | Some u ->
            let ws_url =
                match props.State.LinkConfig with
                | Success config -> config.ws_url
                | _ -> ""
            let s = V2rayN.toString ws_url u.ID u.uuid
            setVs(s)
        ()
    ))
    Mui.dialog [
        dialog.open' (props.State.QRCode <> None)
        dialog.onClose (fun _ -> SetQRCode None |> props.Dispatch)
        prop.children [
            Mui.dialogTitle [
                Html.text "ssss"
            ]
            Mui.dialogContent [
                QRCode ({
                    value = vs
                    size = 300
                    renderAs = "canvas"
                })
            ]
            Mui.dialogActions [
                CopyToClipboard [
                    CopyToClipboardProps.text vs
                    prop.children (
                        Mui.button [
                            prop.text "复制"
                        ]
                    )
                ]
                Mui.button [
                    prop.onClick (fun _ -> SetQRCode None |> props.Dispatch)
                    prop.text "关闭"
                ]
            ]
        ]
    ]
)

type UUIDProps = {
    User: User
    Dispatch: Msg -> unit
}
let renderUUID = React.functionComponent(fun (props: UUIDProps) ->
    let user = props.User
    React.fragment [
        Html.code user.uuid
        Mui.iconButton [
            prop.onClick (fun _->SetQRCode(Some(user)) |> props.Dispatch)
            prop.children (centerFocusWeakIcon [])
        ]
    ]
)

let renderRow (user: User) (dispatch: Msg -> unit) =
    Mui.tableRow [
        prop.key user.ID
        prop.children [
            Mui.tableCell [ prop.text user.ID ]
            Mui.tableCell user.email
            Mui.tableCell [ renderRemark ({ User = user; Dispatch = dispatch }) ]
            Mui.tableCell [ renderUUID ({ User = user; Dispatch = dispatch }) ]
            Mui.tableCell [ renderDisableBtn ({ User = user; Dispatch = dispatch }) ]
            Mui.tableCell "todo"
        ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
    match state.Users with
    | Loading -> Mui.linearProgress []
    | NotAsked -> Html.none
    | Failure e ->
        Mui.alert [
            alert.severity.error
            prop.children [
                Mui.alertTitle [ prop.text "加载用户失败" ]
                Html.text e
            ]
        ]
    | Success users ->
        React.fragment [
            renderQRDialog ({ State = state; Dispatch = dispatch })
            Mui.table [
                Mui.tableHead [
                    Mui.tableRow [
                        Mui.tableCell "ID"
                        Mui.tableCell "邮箱"
                        Mui.tableCell "备注"
                        Mui.tableCell "UUID"
                        Mui.tableCell "状态"
                        Mui.tableCell "操作"
                    ]
                ]
                Mui.tableBody (users |> List.map (fun u -> renderRow u dispatch))
            ]
        ]
