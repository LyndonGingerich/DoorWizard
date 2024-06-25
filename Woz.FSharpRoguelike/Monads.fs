﻿namespace Library

// Maybe (option)
module Maybe =
    type maybeFactory() =
        member this.Bind(monad, func) = Option.bind func monad
        member this.Return(value) = Some value

    let maybe = maybeFactory ()

// Result

type result<'a> =
    | Valid of 'a
    | Invalid of string

module Result =
    let bind func monad =
        match monad with
        | Valid value -> func value
        | Invalid error -> Invalid error

    let (>>=) monad func = bind func monad

    type resultFactory() =
        member this.Bind(monad, func) = monad >>= func
        member this.Return(value) = Valid value

    type resultOrElseFactory() =
        member this.ReturnFrom(x) = x

        member this.Combine(firstMonad, secondMonad) =
            match firstMonad with
            | Valid _ -> firstMonad
            | Invalid _ -> secondMonad

        member this.Delay(f) = f ()

    let result = resultFactory ()
    let resultOrElse = resultOrElseFactory ()
