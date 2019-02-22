%{
open Syntax
exception ParseError
%}

%token <int> INT
%token <float> FLOAT
%token COMMA
%token FFLOOR
%token EOF
%token ADDI 
%token NOP
%token INC
%token DEC
%token AND
%token OR
%token XOR
%token FSQRT
%token ADD
%token SUBI 
%token SUB
%token SLAWI
%token SRAWI
%token FADD
%token FADDMUL2
%token FADDMUL3
%token FADDSQUARE
%token FSUB
%token FMUL
%token FDIV
%token MR
%token LI
%token BLT
%token BGT
%token BGE
%token BNE
%token LIL
%token LIS
%token LIW
%token FLI
%token FORK
%token FORKI
%token JOIN
%token FETCH
%token STORE
%token LOAD
%token ITOF
%token FTOI
%token CMPDI
%token CMPD
%token CMPF
%token BL
%token BEQ
%token BLE
%token BLR
%token BLRR
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
%token <string> ATATIDENT
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
    | label { 
        let t =  $1 in
        if String.length t < 2 then Label(t) else 
        (
            let t = String.sub t 0 2 in
            if t = "@@" then LocalLabel($1) else 
                (
            let t = String.sub t 0 1 in
            if t = "." then 
                LocalLabel($1) else
            Label($1))
        )
        }
    | ADDI reg COMMA reg COMMA INT { if $6 >= 0 then ADDI($2,$4,$6) else  SUBI($2,$4,-$6)}
    | SUBI reg COMMA reg COMMA INT { if $6 >= 0 then SUBI($2,$4,$6) else  ADDI($2,$4,-$6)}
    | AND reg COMMA reg COMMA reg { AND($2,$4,$6) }
    | OR reg COMMA reg COMMA reg { OR($2,$4,$6) }
    | XOR reg COMMA reg COMMA reg { XOR($2,$4,$6) }
    | FSQRT reg COMMA reg { FSQRT($2,$4) }
    | FFLOOR reg COMMA reg { FFLOOR($2,$4) }
    | ADD reg COMMA reg COMMA reg { ADD($2,$4,$6) }
    | SUB reg COMMA reg COMMA reg { SUB($2,$4,$6) }
    | FADD reg COMMA reg COMMA reg { FADD($2,$4,$6) }
    | FMUL reg COMMA reg COMMA reg { FMUL($2,$4,$6) }
    | FSUB reg COMMA reg COMMA reg { FSUB($2,$4,$6) }
    | FDIV reg COMMA reg COMMA reg { FDIV($2,$4,$6) }
    | LIS reg COMMA INT { LIS($2,$4)}
    | FLI reg COMMA FLOAT { FLI($2,$4)}
    | LI reg COMMA INT { LI($2,$4)}
    | LIW reg COMMA INT { LIW($2,$4) }
    | LIL reg COMMA IDENT { LIL($2,$4)}
    | LOAD reg COMMA reg COMMA INT { LOAD($2,$4,$6)}
    | STORE reg COMMA reg COMMA INT { STORE($2,$4,$6)}
    | CMPDI reg COMMA INT { CMPDI($2,$4)}
    | CMPD reg COMMA reg { CMPD($2,$4)}
    | CMPF reg COMMA reg { CMPF($2,$4)}
    | INC reg { ADDI($2,$2,1)}
    | MR reg COMMA reg { ADDI($2,$4,0)}
    | DEC reg { SUBI($2,$2,1)}
    | ITOF reg COMMA reg { ITOF($2,$4)}
    | FTOI reg COMMA reg { FTOI($2,$4)}
    | BEQ IDENT { BEQ($2)}
    | BNE IDENT { BNE($2)}
    | BLE IDENT { BLE($2)}
    | BLT IDENT { BLT($2)}
    | BGE IDENT { BGE($2)}
    | FADDSQUARE reg COMMA reg COMMA reg COMMA reg COMMA reg COMMA reg COMMA reg  { FADDSQUARE($2, $4, $6, $8, $10, $12, $14)}
    | FADDMUL3 reg COMMA reg COMMA reg COMMA reg COMMA reg COMMA reg COMMA reg  { FADDMUL3($2, $4, $6, $8, $10, $12, $14)}
    | FADDMUL2 reg COMMA reg COMMA reg COMMA reg COMMA reg  { FADDMUL2($2, $4, $6, $8, $10 )}
    | BGT IDENT { BGT($2)}
    | BL IDENT { BL($2)}
    | JUMP IDENT {JUMP($2)}
    | BLRR reg { BLRR($2)}
    | BLR { BLR}
    | END { END}
    | INLL reg { IN($2,LL)}
    | INLH reg { IN($2,LH)}
    | INUL reg { IN($2,UL)}
    | INUH reg { IN($2,UH)}
    | NOP { NOP} 
    | OUTLL reg { OUT($2,LL)}
    | OUTLH reg { OUT($2,LH)}
    | OUTUL reg { OUT($2,UL)}
    | OUTUH reg { OUT($2,UH)}
    | SLAWI reg COMMA reg COMMA INT  { SLAWI($2,$4,$6)}
    | SRAWI reg COMMA reg COMMA INT { SRAWI($2,$4,$6)}
    | FETCH reg COMMA reg COMMA reg { FETCH($2, $4, $6)}
    | FORK reg COMMA INT {FORKI($2, $4)}
    | JOIN reg {JOIN($2)}
    | error 
    {
    let lex = (Parsing.symbol_end_pos ()).Lexing.pos_lnum in
         failwith ("syntax error at line " ^ string_of_int lex)
    }

