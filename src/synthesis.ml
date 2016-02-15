open Html5
open Cmdliner

let script = function
  | Some file ->
      [M.script ~a:[M.a_src @@ Xml.uri_of_string file] @@ M.pcdata ""]
  | None -> []

let head =
  let title = M.title @@ M.pcdata "DSH" in
  let charset = M.meta ~a:[M.a_charset "utf-8"] () in
  M.head title (charset :: [])

let body script =
  M.body script

let (?|) f = function
  | Some a -> [f a]
  | None -> []

let main (file : string option) =
  let output = print_string in
  let content = M.html head (body (script file)) in
  P.print ~output content; `Ok ()

let script =
  let doc = "Script js" in
  Arg.(value & opt (some string) None & info ["s"; "script"] ~docv:"script" ~doc)

let cmd =
  let doc = "Rendering html with script" in
  let man = [
    `P "BUGS";
    `S "Email them to <romain.calascibetta@gmail.com";
  ] in
  Term.(ret (pure main $ script)), Term.info "index" ~version:"0.1" ~doc ~man

let () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0
