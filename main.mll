(*run `ocamllex main.mll && ocaml main.ml`
alternatively, just run make*)
{
type annay_lang_tokens = 
|   INT of int
|   IDENT of string
|   ERROR of char
}

let whitespace = [' ''\t''\n']+
let ident_alphanumerics = ['A'-'Z' 'a'-'z' '0'-'9' ''' '_']

rule annay_lang_tokenizer = parse
|   whitespace      {annay_lang_tokenizer lexbuf} (*<rule name> lexbuf to skip this token *)
|   ['a'-'z''_'] ident_alphanumerics* as id {IDENT id}
|   _ as e {ERROR e}


{

    let lexbufr = Lexing.from_channel stdin in
    while true do
        match annay_lang_tokenizer lexbufr with
            | INT i -> print_endline ("integer: " ^ string_of_int i)
            | IDENT id -> print_endline ("identifier: " ^ id)
            | ERROR e -> print_endline ("invalid token character: " ^ (String.make 1 e))
        ;
    done;

}