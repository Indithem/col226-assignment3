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
| whitespace {main_rule lexbuf} (* <rule name> lexbuf to skip this token *)
| "hello" {HELLO}
| "hi" {print_endline "got an hi"; HI}
| eof {Error}
| _ {Error}


{
    let lexbufr = Lexing.from_channel stdin in
    try
        while true do
            match main_rule lexbufr with
                    HELLO ->           print_endline "parsed an hello"
                |   HI ->              print_endline "parsed an hi"
                |   WHITESPACE->       print_endline "shouldn't've got this"
                |   Error->            print_endline "error?!"
            ;
        done;
    with _ ->
        print_endline "Parsing complete"

}