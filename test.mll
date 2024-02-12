{
(* Header - definition for all types that will be used *)

}

let whitespace = [' ''\t']+

rule main_rule = parse
| whitespace {print_endline "whitespace"}
| "hello" {print_endline "got an hello"}


{
    let lexbuf = Lexing.from_channel stdin in
    let result = main_rule lexbuf in
        print_endline "Parsing complete"
}