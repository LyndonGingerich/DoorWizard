namespace Library

type OperationResult<'a> =
    { Contents: 'a option
      Messages: string list }

module OperationResult =
    let success success =
        { Contents = Some success
          Messages = [] }

    let failure msg = { Contents = None; Messages = [ msg ] }

    let ofOption failureMsg maybeContents =
        { Contents = maybeContents
          Messages =
            [ if maybeContents.IsNone then
                  yield failureMsg ] }

    let bind f ma =
        match ma.Contents with
        | Some a ->
            let mb = f a

            { mb with
                Messages = ma.Messages @ mb.Messages }
        | None ->
            { Contents = None
              Messages = ma.Messages }

    let ofTuple (contents, messages) =
        { Contents = Some contents
          Messages = messages }

[<AutoOpen>]
module ResultFactory =
    open OperationResult

    type resultFactory() =
        member this.Bind(m, f) = bind f m
        member this.Return(value) = success value
        member this.ReturnFrom(value) = value

    let result = resultFactory ()
