module Components

open Fable.Core.JsInterop
open Feliz
open Feliz.MaterialUI

type QRCodeProps =
    { value: string
      size: int
      renderAs: string }

let QRCode (props: QRCodeProps): ReactElement = importDefault "qrcode.react"


type CopyToClipboardProps =
    static member inline text(value: string) = Interop.mkAttr "text" value
    static member inline onCopy(value: string * bool -> unit) = Interop.mkAttr "onCopy" value

let CopyToClipboard props =
    createElement (importDefault "react-copy-to-clipboard") props
