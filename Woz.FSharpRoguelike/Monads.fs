namespace Library

module Result =
    let bind func monad =
        match monad with
        | Ok value -> func value
        | Error error -> Error error

    type resultFactory() =
        member this.Bind(monad, func) = bind func monad
        member this.Return(value) = Ok value
        member this.ReturnFrom(value) = value

    let result = resultFactory ()
