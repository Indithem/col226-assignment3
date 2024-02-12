(*run `ocamllex test.mll && ocaml test.ml`*)
{
(* Header - definition for all types that will be used *)

}

let whitespace = [' ''\t']+

rule main_rule = parse
| whitespace {print_endline "whitespace"}
| "hello" {print_endline "got an hello"}
| "hi" {print_endline "got an hi"}
| eof {print_endline "bye bye parsing!"}


{
    let lexbuf = Lexing.from_channel stdin in
    try
        while true do
            main_rule lexbuf
        done;
    with _ ->
        print_endline "Parsing complete"

}