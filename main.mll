(*run `ocamllex main.mll && ocaml main.ml`
alternatively, just run make*)
{
type annay_lang_tokens = 
|   INT of int
|   IDENT of string
|   ERROR of char | EOF
|   TERNARY_BRANCH
|   IF | THEN | ELSE | WHILE | DO | FOR | IN
|   TUPLE | TUPLE_FIRST | TUPLE_SECOND
|   BOOL_TRUE | BOOL_FALSE
|   AND | OR | NOT | XOR | NAND | NOR
|   ADD | SUB | MUL | DIV | MOD | POW
|   GT | LT | EQ | GTE | LTE | NE
|   STRING of string
|   STRING_CONCAT | STRING_CONTAINS
|   COMMA | PERIOD | QUESTION_MARK
|   PARENTHESIS_OPEN | PARENTHESIS_CLOSE
|   SQU_PARANTHESIS_OPEN | SQU_PARANTHESIS_CLOSE
|   FL_PARANTHESIS_OPEN | FL_PARANTHESIS_CLOSE
}

let whitespace = [' ''\t''\n']
let ident_alphanumerics = ['A'-'Z' 'a'-'z' '0'-'9' ''' '_']

rule annay_lang_tokenizer = parse
|   whitespace+      {annay_lang_tokenizer lexbuf} (*<rule name> lexbuf to skip this token *)
|   ['a'-'z''_'] ident_alphanumerics* as id {IDENT id}
|   "if-then-else"  {TERNARY_BRANCH}

|   "tuple"         {TUPLE}
|   "first"         {TUPLE_FIRST}
|   "second"        {TUPLE_SECOND}

|   '"' (_+ as s) '"' {STRING s}
| ''' (_ as s) '''  {STRING (String.make 1 s)}

|   _ as e          {ERROR e}
|   eof             {EOF}


{

    let lexbufr = Lexing.from_channel stdin in
    let cond = ref true in
    while !cond do
        match annay_lang_tokenizer lexbufr with
            | INT i -> print_endline ("integer: " ^ string_of_int i)
            | IDENT id -> print_endline ("identifier: " ^ id)
            | STRING s -> print_endline ("string: "^ s)
            | ERROR e -> print_endline ("invalid token character: " ^ (String.make 1 e))
            | EOF ->  cond:= false
        ;
    done;
    print_endline "Done!"

}