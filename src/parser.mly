%{
open Syntax
exception ParseError
%}

%token <int> INT
%token <float> FLOAT
%token COMMA
%token EOF
%token ADDI 
%token ADD
%token SUBI 
%token SUB
%token MULI 
%token MUL
%token DIVI
%token DIV
%token MR
%token LI
%token LIS
%token STORE
%token LOAD
%token CMPDI
%token CMPD
%token BL
%token BEQ
%token BLE
%token BLR
%token END
%token INLL
%token INLH
%token INUL
%token INUH
%token OUTLL
%token OUTLH
%token OUTUL
%token OUTUH
%token JUMP
%token COLON
%token <int> PERCENTINT
%token <string> IDENT
%type <Syntax.t list> exp
%start exp
%%
exp:
    | order exp { $1::$2 }
    | EOF {[]}
label: 
    | IDENT COLON {$1}
reg: 
    | PERCENTINT {$1}
order:
    | label  { Label($1) }
    | ADDI reg COMMA reg COMMA INT { ADDI($2,$4,$6) }
    | MULI reg COMMA reg COMMA INT { MULI($2,$4,$6) }
    | SUBI reg COMMA reg COMMA INT { SUBI($2,$4,$6) }
    | DIVI reg COMMA reg COMMA INT { DIVI($2,$4,$6) }
    | ADD reg COMMA reg COMMA reg { ADD($2,$4,$6) }
    | MUL reg COMMA reg COMMA reg { MUL($2,$4,$6) }
    | SUB reg COMMA reg COMMA reg { SUB($2,$4,$6) }
    | DIV reg COMMA reg COMMA reg { DIV($2,$4,$6) }
    | LIS reg COMMA INT { LIS($2,$4)}
    | LI reg COMMA INT { LI($2,$4)}
    | LOAD reg COMMA reg COMMA INT { LOAD($2,$4,$6)}
    | STORE reg COMMA reg COMMA INT { STORE($2,$4,$6)}
    | CMPDI reg COMMA INT { CMPDI($2,$4)}
    | CMPD reg COMMA reg { CMPD($2,$4)}
    | BEQ IDENT { BEQ($2)}
    | BLE IDENT { BLE($2)}
    | BLR { BLR}
    | BL IDENT { BL($2)}
    | END { END}
    | INLL reg { IN($2,LL)}
    | INLH reg { IN($2,LH)}
    | INUL reg { IN($2,UL)}
    | INUH reg { IN($2,UH)}
    | OUTLL reg { OUT($2,LL)}
    | OUTLH reg { OUT($2,LH)}
    | OUTUL reg { OUT($2,UL)}
    | OUTUH reg { OUT($2,UH)}
    | JUMP IDENT {JUMP($2)}
    | error 
    {
    let lex = (Parsing.symbol_end_pos ()).Lexing.pos_lnum in
         failwith ("syntax error at line " ^ string_of_int lex)
    }

