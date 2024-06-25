namespace Library

module Result =
    let bind func monad =
        match monad with
        | Ok value -> func value
        | Error error -> Error error

    type resultFactory() =
        member this.Bind(monad, func) = bind func monad
        member this.Return(value) = Ok value

    type resultOrElseFactory() =
        member this.ReturnFrom(x) = x

        member this.Combine(firstMonad, secondMonad) =
            match firstMonad with
            | Ok _ -> firstMonad
            | Error _ -> secondMonad

        member this.Delay(f) = f ()

    let result = resultFactory ()
    let resultOrElse = resultOrElseFactory ()
