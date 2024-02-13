(*run `ocamllex main.mll && ocaml main.ml`
alternatively, just run make*)
{
type annay_lang_tokens = 
|   INT of int
|   IDENT of string
|   ASSIGNMENT | SEMICOLON
|   ERROR of string | EOF
|   TERNARY_BRANCH
|   IF | THEN | ELSE | WHILE | DO | FOR | IN | LET
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
let alphanumerics = ['A'-'Z' 'a'-'z' '0'-'9']

let triple_quote = '"' '"' '"'
let literal_without_triple_quote = [^'"' '"' '"']

rule annay_lang_tokenizer = parse
|   whitespace+      {annay_lang_tokenizer lexbuf} (*<rule name> lexbuf to skip this token *)
|   "if-then-else"  {TERNARY_BRANCH}

|   "tuple"           {TUPLE}
|   "first"           {TUPLE_FIRST}
|   "second"          {TUPLE_SECOND}

| '"' ([^'\n''"']+ as s) '"'   {STRING s}
| ''' ([^'\n' ''']+ as s) '''  {STRING s}
| triple_quote (literal_without_triple_quote+ as s) triple_quote {STRING s}
| ['1'-'9']['0'-'9']* as i   {INT (int_of_string i)}
| '0'                 {INT 0}

| "if"             {IF}
| "then"           {THEN}
| "else"           {ELSE}
| "while"          {WHILE}
| "do"             {DO}
| "for"            {FOR}
| "in"             {IN}
| "let"            {LET}

| "true"            {BOOL_TRUE}
| "false"           {BOOL_FALSE}

| '&'whitespace* '&' {AND}
| '|'whitespace* '|' {OR}
| '/'whitespace* '\\' {NOT}
| '^'whitespace* '^' {XOR}
| '/'whitespace* '&' {NAND}
| '/'whitespace* '|' {NOR}

| '+'               {ADD}
| '-'               {SUB}
| '*'               {MUL}
| '/'               {DIV}
| '%'               {MOD}
| '^'               {POW}

| '>'               {GT}
| '<'               {LT}
| '='whitespace* '?'{EQ}
| '>'whitespace* '='{GTE}
| '<'whitespace* '='{LTE}
| '/'whitespace* '='{NE}

| '@'               {STRING_CONCAT}
| '('whitespace*'='whitespace*'?' {STRING_CONTAINS}

| ','               {COMMA}
| '.'               {PERIOD}
| '?'               {QUESTION_MARK}

| '('               {PARENTHESIS_OPEN}
| ')'               {PARENTHESIS_CLOSE}
| '['               {SQU_PARANTHESIS_OPEN}
| ']'               {SQU_PARANTHESIS_CLOSE}
| '{'               {FL_PARANTHESIS_OPEN}
| '}'               {FL_PARANTHESIS_CLOSE}

| ':'whitespace* '='{ASSIGNMENT}
| ';'               {SEMICOLON}

|   '+'             {ADD}
|   '-'             {SUB}
|   '*'             {MUL}
|   '/'             {DIV}
|   '%'             {MOD}
|   '^'             {POW}

|   ['a'-'z''_'] ident_alphanumerics* as id {IDENT id}

|   (alphanumerics+ as e)  {ERROR e}
|   _ as e          {ERROR (String.make 1 e)}
|   eof             {EOF}


{

    let lexbufr = Lexing.from_channel stdin in
    let cond = ref true in
    while !cond do
        match annay_lang_tokenizer lexbufr with
            | INT i -> print_endline ("integer: " ^ string_of_int i)
            | IDENT id -> print_endline ("identifier: " ^ id)
            | STRING s -> print_endline ("string: "^ s)
            
            | TERNARY_BRANCH -> print_endline ("if-then-else ternary branch")
            | TUPLE -> print_endline ("tuple")
            | TUPLE_FIRST -> print_endline "tuple operator first"
            | TUPLE_SECOND -> print_endline "tuple operator second"
            
            | IF -> print_endline "keyword if"
            | THEN -> print_endline "keyword then"
            | ELSE -> print_endline "keyword else"
            | WHILE -> print_endline "keyword while"
            | DO -> print_endline "keyword do"
            | FOR -> print_endline "keyword for"
            | IN -> print_endline "keyword in"
            | LET -> print_endline "keyword let"

            | AND -> print_endline "boolean operator and"
            | OR -> print_endline "boolean operator or"
            | NOT -> print_endline "boolean operator not"
            | XOR -> print_endline "boolean operator xor"
            | NAND -> print_endline "boolean operator nand"
            | NOR -> print_endline "boolean operator nor"

            | ASSIGNMENT -> print_endline "assignment operator"
            | SEMICOLON -> print_endline "semicolon"

            | BOOL_TRUE -> print_endline "boolean constant true"
            | BOOL_FALSE -> print_endline "boolean constant false"

            | ADD -> print_endline "integer addition operator"
            | SUB -> print_endline "integer subtraction operator"
            | MUL -> print_endline "integer multiplication operator"
            | DIV -> print_endline "integer division operator"
            | MOD -> print_endline "integer modulo operator"
            | POW -> print_endline "integer power operator"

            | GT -> print_endline "integer greater than comparision operator"
            | LT -> print_endline "integer less than comparision operator"
            | EQ -> print_endline "integer equality comparision operator"
            | GTE -> print_endline "integer greater than or equal to comparision operator"
            | LTE -> print_endline "integer less than or equal to comparision operator"
            | NE -> print_endline "integer not equal to comparision operator"

            | STRING_CONCAT -> print_endline "string concatenation operator"
            | STRING_CONTAINS -> print_endline "string contains operator"

            | COMMA -> print_endline "comma"
            | PERIOD -> print_endline "period"
            | QUESTION_MARK -> print_endline "question mark"

            | PARENTHESIS_OPEN -> print_endline "parenthesis open"
            | PARENTHESIS_CLOSE -> print_endline "parenthesis close"
            | SQU_PARANTHESIS_OPEN -> print_endline "square parenthesis open"
            | SQU_PARANTHESIS_CLOSE -> print_endline "square parenthesis close"
            | FL_PARANTHESIS_OPEN -> print_endline "flower parenthesis open"
            | FL_PARANTHESIS_CLOSE -> print_endline "flower parenthesis close"

            | ERROR e -> print_endline ("invalid token : " ^ e)
            | EOF ->  cond:= false
        ;
    done;
    print_endline "Done Lexing!"

}


(*test_cases1.ay
"hello"
'hi'
'h'

let m :=tuple h1 h3
fisrt m    first m
second m

let x := true;

true && false
true /        \ false
if true then 00003211 else 2 ;
else 2;

firstme
tuple;me

19990

*)

(*test_cases2.ay
1abc

24;Aabc;ABC123;abc'sad'213'ff

123&        &23 gs34 
"&&" '"a"'

"""
'hello'
guys
"""
*)

(*test_cases3.ay
({[abc++123p^"hello"]})

pa''sd < == ? 123
*)