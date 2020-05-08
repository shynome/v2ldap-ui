module Shared.Types

type Response<'a> =
    { message: string option
      error: string option
      data: 'a option }

type User =
    { ID: int
      version: int
      email: string
      uuid: string
      remark: string
      disabled: bool }
