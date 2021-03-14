open Cmdliner

let cmds = [Cmd_hello.cmd]

(* Command line interface *)

let doc = "A library and CLI for the decoding and encoding of Roman numerals"

let sdocs = Manpage.s_common_options

let exits = Common.exits

let envs = Common.envs

let man =
  [ `S Manpage.s_description
  ; `P "A library and CLI for the decoding and encoding of Roman numerals"
  ; `S Manpage.s_commands
  ; `S Manpage.s_common_options
  ; `S Manpage.s_exit_status
  ; `P "These environment variables affect the execution of $(mname):"
  ; `S Manpage.s_environment
  ; `S Manpage.s_bugs
  ; `P "File bug reports at $(i,%%PKG_ISSUES%%)"
  ; `S Manpage.s_authors
  ; `P "Noah Summers, $(i,https://github.com/noahsummers)" ]

let default_cmd =
  let term =
    let open Common.Let_syntax in
    Term.ret
    @@ let+ _ = Common.term in
       `Help (`Pager, None)
  in
  let info =
    Term.info "romanum" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man ~envs
  in
  (term, info)

let () = Term.(exit_status @@ eval_choice default_cmd cmds)
