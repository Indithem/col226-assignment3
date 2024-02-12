(*run `ocamllex test.mll && ocaml test.ml`*)
{
(* Header - definition for all types that will be used *)
type custom_token= 
| HELLO
| HI
| WHITESPACE
| Error
}

let whitespace = [' ''\t']+

rule main_rule = parse
| whitespace {WHITESPACE}
| "hello" {HELLO}
| "hi" {print_endline "got an hi"; HI}
| eof {Error}


{
    let lexbuf = Lexing.from_channel stdin in
    try
        while true do
            main_rule lexbuf
        done;
    with _ ->
        print_endline "Parsing complete"

}