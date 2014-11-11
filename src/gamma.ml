type t = Type.t Map.Make(String).t

module G = Map.Make(String)

let add key value gamma = match value with
  | _ -> G.add key value gamma

let lookup key gamma = G.find key gamma

let empty = G.empty;
