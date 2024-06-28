﻿namespace Library

module Result =
    let ofOption error =
        function
        | Some a -> Ok a
        | None -> Error error
