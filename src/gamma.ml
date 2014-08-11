type t =
  {
    alias    : Type.t Map.Make(String).t;
    datatype : Type.t Map.Make(String).t;
  }

module G = Map.Make(String)

module Alias = struct
  let lookup key { alias; _ } = G.find key alias
end

module Datatype = struct
  let exists ctor { datatype; _ } =
    let exists ctor map =
      G.exists (fun _ -> function
          | Type.Set l -> Type.Set.mem ctor l
          | _ -> raise (Invalid_argument "Gamma.Datatype.lookup"))
        map
    in
    exists ctor datatype

  let lookup ctor { datatype; _ } =
    let filter ctor map =
      G.filter (fun _ -> function
          | Type.Set l -> Type.Set.mem ctor l
          | _ -> raise (Invalid_argument "Gamma.Datatype.lookup"))
        map
    in
    G.choose (filter ctor datatype)
end

exception Constructor_already_exists of string

let () = Printexc.register_printer
    (function
      | Constructor_already_exists ctor ->
        Some (Printf.sprintf "constructor %s already exists" ctor)
      | _ -> None)

let add key value gamma = match value with
  | Type.Set l ->
    let exists = Type.Set.fold
        (fun ctor _ ->
           function
           | None -> if Datatype.exists ctor gamma
             then Some ctor
             else None
           | x -> x)
        l None
    in exists |> (function
      | Some ctor -> raise (Constructor_already_exists ctor)
      | None -> { gamma with datatype = G.add key value gamma.datatype })
  | _ -> { gamma with alias = G.add key value gamma.alias }

let lookup key { alias; datatype; } =
  try G.find key alias
  with Not_found -> G.find key datatype

let empty =
  {
    alias = G.empty;
    datatype = G.empty;
  }
