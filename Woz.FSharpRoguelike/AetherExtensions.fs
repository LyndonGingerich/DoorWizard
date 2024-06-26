namespace Library

open Aether

module Optics =

    module Map =
        let expectValue_ (k: 'k) : Lens<Map<'k, 'v>, 'v> = Map.find k, Map.add k

    module List =
        let notEmpty_: Isomorphism<List<'a> option, List<'a>> =
            (function
            | Some list -> list
            | None -> []),
            (fun list ->
                match Seq.isEmpty list with
                | true -> None
                | false -> Some list)

        let where_ predicate : Lens<list<'a>, 'a option> =
            List.tryFind predicate,
            (fun itemOption list ->
                let cleanList = list |> List.filter (predicate >> not)

                match itemOption with
                | Some item -> item :: cleanList
                | None -> cleanList)

        let expectWhere_ predicate : Lens<list<'a>, 'a> =
            List.find predicate,
            (fun item list ->
                let cleanList = list |> List.filter (predicate >> not)
                item :: cleanList)
