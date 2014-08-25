{
  exception Error
}

let digit = [ '0' - '9' ]
let number = digit+
let ident = [ 'a' - 'z' ] [ 'a' - 'z' 'A' - 'Z' '_' ]*

rule token = parse
  | eof                    { Parser.EOF }
  | '['                    { Parser.LBRA }
  | ']'                    { Parser.RBRA }
  | '*'                    { Parser.STAR }
  | '+'                    { Parser.PLUS }
  | '/'                    { Parser.UPON }
  | '('                    { Parser.LPAR }
  | ')'                    { Parser.RPAR }
  | '|'                    { Parser.PIPE }
  | '-'                    { Parser.MINUS }
  | ':'                    { Parser.COLON }
  | ';'                    { Parser.SEMICOLON }
  | '='                    { Parser.EQUAL }
  | '<'                    { Parser.LESS }
  | "<="                   { Parser.LESSEQ }
  | '>'                    { Parser.MORE }
  | ">="                   { Parser.MOREEQ }
  | '!'                    { Parser.BANG }
  | '&'                    { Parser.AMPERSAND }
  | '%'                    { Parser.PERCENT }
  | "or" | "||"            { Parser.OR }
  | "and" | "&&"           { Parser.AND }
  | "true"                 { Parser.TRUE }
  | "false"                { Parser.FALSE }
  | ident as n             { Parser.IDENT n }
  | number as n            { Parser.NUMBER (int_of_string n) }
  | [ ' ' '\t' '\r' '\n' ] { token lexbuf }
  | _                      { raise Error }

{

}
