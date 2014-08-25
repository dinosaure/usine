%{

%}

%token EOF

%token <string> IDENT
%token <int> NUMBER

%token TRUE FALSE
%token LESS MORE LESSEQ MOREEQ OR AND BANG
%token PLUS MINUS STAR UPON PERCENT
%token EQUAL PIPE COLON SEMICOLON AMPERSAND

%token LPAR RPAR RBRA LBRA

%left AND OR
%nonassoc Not
%left PLUS MINUS
%left STAR UPON PERCENT
%nonassoc Neg

%start final
%type <int list> final

%%

slist(X, C):
  | x = X C r = slist(X, C)
  { x :: r }
  | x = X
  { [ x ] }

aexpr:
  | x = NUMBER
  { fun _ -> x }
  | _ = IDENT
  { fun e -> e }
  | a = aexpr PLUS b = aexpr
  { fun e -> (a e) + (b e) }
  | a = aexpr MINUS b = aexpr
  { fun e -> (a e) - (b e) }
  | a = aexpr STAR b = aexpr
  { fun e -> (a e) * (b e) }
  | a = aexpr UPON b = aexpr
  { fun e -> (a e) / (b e) }
  | a = aexpr PERCENT b = aexpr
  { fun e -> (a e) mod (b e) }
  | MINUS x = aexpr
  { fun e -> - (x e) } %prec Neg
  | LPAR x = aexpr RPAR
  { x }

bexpr:
  | TRUE { fun _ -> true }
  | FALSE { fun _ -> false }
  | a = aexpr EQUAL b = aexpr
  { fun e -> (a e) = (b e) }
  | a = aexpr LESS b = aexpr
  { fun e -> (a e) < (b e) }
  | a = aexpr MORE b = aexpr
  { fun e -> (a e) > (b e) }
  | a = aexpr LESSEQ b = aexpr
  { fun e -> (a e) <= (b e) }
  | a = aexpr MOREEQ b = aexpr
  { fun e -> (a e) >= (b e) }
  | a = bexpr OR b = bexpr
  { fun e -> (a e) || (b e) }
  | a = bexpr AND b = bexpr
  { fun e -> (a e) && (b e) }
  | BANG x = bexpr
  { fun e -> not (x e) } %prec Not
  | LPAR x = bexpr RPAR
  { x }

predicat:
  | x = bexpr
  { fun s -> Batteries.Enum.filter x s }

enumeration:
  | a = NUMBER COLON b = NUMBER
  { Batteries.Enum.range a ~until:b }

expression:
  | x = aexpr
  { fun s -> Batteries.Enum.map x s }

final:
  | LBRA
      expr = expression PIPE
      enum = enumeration AMPERSAND
      pred = separated_list(SEMICOLON, predicat)
    RBRA EOF
  { Batteries.List.of_enum
      (expr (List.fold_right (fun f s -> f s) pred enum)) }

