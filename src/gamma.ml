type t =
  {
    alias    : Type.t Map.Make(String).t;
  }

module G = Map.Make(String)

let add key value gamma = match value with
  | _ -> { alias = G.add key value gamma.alias }

let lookup key { alias; } = G.find key alias

let empty =
  {
    alias = G.empty;
  }
