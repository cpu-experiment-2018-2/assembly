type token =
  | INT of (int)
  | FLOAT of (float)
  | COMMA
  | FFLOOR
  | EOF
  | ADDI
  | NOP
  | INC
  | DEC
  | AND
  | OR
  | XOR
  | FSQRT
  | ADD
  | SUBI
  | SUB
  | SLAWI
  | SRAWI
  | FADD
  | FSUB
  | FMUL
  | FDIV
  | MR
  | LI
  | BLT
  | BGT
  | BGE
  | BNE
  | LIL
  | LIS
  | LIW
  | FLI
  | STORE
  | LOAD
  | ITOF
  | FTOI
  | CMPDI
  | CMPD
  | CMPF
  | BL
  | BEQ
  | BLE
  | BLR
  | BLRR
  | END
  | INLL
  | INLH
  | INUL
  | INUH
  | OUTLL
  | OUTLH
  | OUTUL
  | OUTUH
  | JUMP
  | COLON
  | PERCENTINT of (int)
  | IDENT of (string)
  | ATATIDENT of (string)

open Parsing;;
let _ = parse_error;;
# 2 "src/parser.mly"
open Syntax
exception ParseError
# 67 "src/parser.ml"
let yytransl_const = [|
  259 (* COMMA *);
  260 (* FFLOOR *);
    0 (* EOF *);
  261 (* ADDI *);
  262 (* NOP *);
  263 (* INC *);
  264 (* DEC *);
  265 (* AND *);
  266 (* OR *);
  267 (* XOR *);
  268 (* FSQRT *);
  269 (* ADD *);
  270 (* SUBI *);
  271 (* SUB *);
  272 (* SLAWI *);
  273 (* SRAWI *);
  274 (* FADD *);
  275 (* FSUB *);
  276 (* FMUL *);
  277 (* FDIV *);
  278 (* MR *);
  279 (* LI *);
  280 (* BLT *);
  281 (* BGT *);
  282 (* BGE *);
  283 (* BNE *);
  284 (* LIL *);
  285 (* LIS *);
  286 (* LIW *);
  287 (* FLI *);
  288 (* STORE *);
  289 (* LOAD *);
  290 (* ITOF *);
  291 (* FTOI *);
  292 (* CMPDI *);
  293 (* CMPD *);
  294 (* CMPF *);
  295 (* BL *);
  296 (* BEQ *);
  297 (* BLE *);
  298 (* BLR *);
  299 (* BLRR *);
  300 (* END *);
  301 (* INLL *);
  302 (* INLH *);
  303 (* INUL *);
  304 (* INUH *);
  305 (* OUTLL *);
  306 (* OUTLH *);
  307 (* OUTUL *);
  308 (* OUTUH *);
  309 (* JUMP *);
  310 (* COLON *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  311 (* PERCENTINT *);
  312 (* IDENT *);
  313 (* ATATIDENT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\004\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\001\000\001\000\006\000\006\000\006\000\
\006\000\006\000\004\000\004\000\006\000\006\000\006\000\006\000\
\006\000\006\000\004\000\004\000\004\000\004\000\004\000\006\000\
\006\000\004\000\004\000\004\000\002\000\004\000\002\000\004\000\
\004\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\001\000\001\000\002\000\002\000\002\000\002\000\
\001\000\002\000\002\000\002\000\002\000\006\000\006\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\056\000\000\000\002\000\000\000\049\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\043\000\000\000\044\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\057\000\
\000\000\005\000\004\000\000\000\000\000\029\000\031\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\037\000\039\000\
\038\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\040\000\034\000\036\000\
\042\000\045\000\046\000\047\000\048\000\050\000\051\000\052\000\
\053\000\041\000\003\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\000\000\000\000\000\000\000\000\000\011\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\030\000\
\021\000\023\000\019\000\022\000\020\000\000\000\000\000\032\000\
\033\000\026\000\027\000\028\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\008\000\009\000\010\000\013\000\
\007\000\014\000\054\000\055\000\015\000\017\000\016\000\018\000\
\025\000\024\000"

let yydgoto = "\002\000\
\056\000\057\000\058\000\060\000"

let yysindex = "\018\000\
\001\000\000\000\000\000\221\254\000\000\221\254\000\000\221\254\
\221\254\221\254\221\254\221\254\221\254\221\254\221\254\221\254\
\221\254\221\254\221\254\221\254\221\254\221\254\221\254\221\254\
\222\254\234\254\235\254\236\254\221\254\221\254\221\254\221\254\
\221\254\221\254\221\254\221\254\221\254\221\254\221\254\237\254\
\239\254\248\254\000\000\221\254\000\000\221\254\221\254\221\254\
\221\254\221\254\221\254\221\254\221\254\249\254\223\254\000\000\
\001\000\000\000\000\000\047\255\048\255\000\000\000\000\049\255\
\050\255\051\255\052\255\053\255\054\255\055\255\056\255\057\255\
\058\255\059\255\060\255\061\255\062\255\063\255\000\000\000\000\
\000\000\000\000\064\255\065\255\066\255\067\255\068\255\069\255\
\070\255\071\255\072\255\073\255\074\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\221\254\221\254\221\254\221\254\
\221\254\221\254\221\254\221\254\221\254\221\254\221\254\221\254\
\221\254\221\254\221\254\221\254\077\255\023\255\079\255\080\255\
\081\255\221\254\221\254\221\254\221\254\083\255\221\254\221\254\
\000\000\082\255\084\255\085\255\086\255\000\000\087\255\088\255\
\089\255\090\255\091\255\092\255\093\255\094\255\095\255\000\000\
\000\000\000\000\000\000\000\000\000\000\096\255\097\255\000\000\
\000\000\000\000\000\000\000\000\100\255\221\254\221\254\221\254\
\221\254\101\255\221\254\118\255\119\255\221\254\221\254\221\254\
\221\254\120\255\121\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\025\000\000\000\000\000\250\255"

let yytablesize = 313
let yytable = "\061\000\
\005\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\001\000\059\000\107\000\079\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\080\000\081\000\082\000\094\000\097\000\095\000\098\000\
\099\000\100\000\101\000\102\000\103\000\104\000\105\000\096\000\
\106\000\109\000\110\000\111\000\112\000\113\000\114\000\115\000\
\116\000\117\000\118\000\119\000\120\000\121\000\122\000\123\000\
\124\000\125\000\126\000\127\000\128\000\129\000\130\000\131\000\
\132\000\133\000\134\000\135\000\136\000\153\000\154\000\155\000\
\156\000\108\000\157\000\162\000\165\000\000\000\166\000\167\000\
\168\000\169\000\170\000\171\000\172\000\173\000\174\000\175\000\
\176\000\177\000\178\000\179\000\180\000\185\000\137\000\138\000\
\139\000\140\000\141\000\142\000\143\000\144\000\145\000\146\000\
\147\000\148\000\149\000\150\000\151\000\152\000\187\000\188\000\
\193\000\194\000\000\000\158\000\159\000\160\000\161\000\000\000\
\163\000\164\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\181\000\
\182\000\183\000\184\000\000\000\186\000\000\000\000\000\189\000\
\190\000\191\000\192\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\003\000\000\000\000\000\000\000\004\000\006\000\007\000\008\000\
\009\000\010\000\011\000\012\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000\
\041\000\042\000\043\000\044\000\045\000\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\000\000\000\000\
\055\000"

let yycheck = "\006\000\
\000\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\020\000\021\000\022\000\
\023\000\024\000\001\000\055\001\054\001\056\001\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\056\001\056\001\056\001\056\001\044\000\056\001\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\056\001\
\056\001\003\001\003\001\003\001\003\001\003\001\003\001\003\001\
\003\001\003\001\003\001\003\001\003\001\003\001\003\001\003\001\
\003\001\003\001\003\001\003\001\003\001\003\001\003\001\003\001\
\003\001\003\001\003\001\003\001\003\001\001\001\056\001\001\001\
\001\001\057\000\002\001\001\001\003\001\255\255\003\001\003\001\
\003\001\003\001\003\001\003\001\003\001\003\001\003\001\003\001\
\003\001\003\001\003\001\003\001\001\001\001\001\109\000\110\000\
\111\000\112\000\113\000\114\000\115\000\116\000\117\000\118\000\
\119\000\120\000\121\000\122\000\123\000\124\000\001\001\001\001\
\001\001\001\001\255\255\130\000\131\000\132\000\133\000\255\255\
\135\000\136\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\166\000\
\167\000\168\000\169\000\255\255\171\000\255\255\255\255\174\000\
\175\000\176\000\177\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\255\255\255\255\255\255\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\041\001\042\001\043\001\044\001\045\001\046\001\047\001\
\048\001\049\001\050\001\051\001\052\001\053\001\255\255\255\255\
\056\001"

let yynames_const = "\
  COMMA\000\
  FFLOOR\000\
  EOF\000\
  ADDI\000\
  NOP\000\
  INC\000\
  DEC\000\
  AND\000\
  OR\000\
  XOR\000\
  FSQRT\000\
  ADD\000\
  SUBI\000\
  SUB\000\
  SLAWI\000\
  SRAWI\000\
  FADD\000\
  FSUB\000\
  FMUL\000\
  FDIV\000\
  MR\000\
  LI\000\
  BLT\000\
  BGT\000\
  BGE\000\
  BNE\000\
  LIL\000\
  LIS\000\
  LIW\000\
  FLI\000\
  STORE\000\
  LOAD\000\
  ITOF\000\
  FTOI\000\
  CMPDI\000\
  CMPD\000\
  CMPF\000\
  BL\000\
  BEQ\000\
  BLE\000\
  BLR\000\
  BLRR\000\
  END\000\
  INLL\000\
  INLH\000\
  INUL\000\
  INUH\000\
  OUTLL\000\
  OUTLH\000\
  OUTUL\000\
  OUTUH\000\
  JUMP\000\
  COLON\000\
  "

let yynames_block = "\
  INT\000\
  FLOAT\000\
  PERCENTINT\000\
  IDENT\000\
  ATATIDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'order) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Syntax.t list) in
    Obj.repr(
# 68 "src/parser.mly"
                ( _1::_2 )
# 396 "src/parser.ml"
               : Syntax.t list))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "src/parser.mly"
          ([])
# 402 "src/parser.ml"
               : Syntax.t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 71 "src/parser.mly"
                  (_1)
# 409 "src/parser.ml"
               : 'label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 73 "src/parser.mly"
                 (_1)
# 416 "src/parser.ml"
               : 'reg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'label) in
    Obj.repr(
# 75 "src/parser.mly"
            ( 
        let t =  _1 in
        if String.length t < 2 then Label(t) else 
        (
            let t = String.sub t 0 2 in
            if t = "@@" then LocalLabel(_1) else Label(_1)
        )
        )
# 430 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 83 "src/parser.mly"
                                   ( if _6 >= 0 then ADDI(_2,_4,_6) else  SUBI(_2,_4,-_6))
# 439 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 84 "src/parser.mly"
                                   ( if _6 >= 0 then SUBI(_2,_4,_6) else  ADDI(_2,_4,-_6))
# 448 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 85 "src/parser.mly"
                                  ( AND(_2,_4,_6) )
# 457 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 86 "src/parser.mly"
                                 ( OR(_2,_4,_6) )
# 466 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 87 "src/parser.mly"
                                  ( XOR(_2,_4,_6) )
# 475 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 88 "src/parser.mly"
                          ( FSQRT(_2,_4) )
# 483 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 89 "src/parser.mly"
                           ( FFLOOR(_2,_4) )
# 491 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 90 "src/parser.mly"
                                  ( ADD(_2,_4,_6) )
# 500 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 91 "src/parser.mly"
                                  ( SUB(_2,_4,_6) )
# 509 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 92 "src/parser.mly"
                                   ( FADD(_2,_4,_6) )
# 518 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 93 "src/parser.mly"
                                   ( FMUL(_2,_4,_6) )
# 527 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 94 "src/parser.mly"
                                   ( FSUB(_2,_4,_6) )
# 536 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 95 "src/parser.mly"
                                   ( FDIV(_2,_4,_6) )
# 545 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 96 "src/parser.mly"
                        ( LIS(_2,_4))
# 553 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 97 "src/parser.mly"
                          ( FLI(_2,_4))
# 561 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 98 "src/parser.mly"
                       ( LI(_2,_4))
# 569 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 99 "src/parser.mly"
                        ( LIW(_2,_4) )
# 577 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "src/parser.mly"
                          ( LIL(_2,_4))
# 585 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 101 "src/parser.mly"
                                   ( LOAD(_2,_4,_6))
# 594 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 102 "src/parser.mly"
                                    ( STORE(_2,_4,_6))
# 603 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 103 "src/parser.mly"
                          ( CMPDI(_2,_4))
# 611 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 104 "src/parser.mly"
                         ( CMPD(_2,_4))
# 619 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 105 "src/parser.mly"
                         ( CMPF(_2,_4))
# 627 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 106 "src/parser.mly"
              ( ADDI(_2,_2,1))
# 634 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 107 "src/parser.mly"
                       ( ADDI(_2,_4,0))
# 642 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 108 "src/parser.mly"
              ( SUBI(_2,_2,1))
# 649 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 109 "src/parser.mly"
                         ( ITOF(_2,_4))
# 657 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 110 "src/parser.mly"
                         ( FTOI(_2,_4))
# 665 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "src/parser.mly"
                ( BEQ(_2))
# 672 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 112 "src/parser.mly"
                ( BNE(_2))
# 679 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 114 "src/parser.mly"
                ( BLE(_2))
# 686 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 115 "src/parser.mly"
                ( BLT(_2))
# 693 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "src/parser.mly"
                ( BGE(_2))
# 700 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "src/parser.mly"
                ( BGT(_2))
# 707 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 119 "src/parser.mly"
               ( BL(_2))
# 714 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "src/parser.mly"
                 (JUMP(_2))
# 721 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 121 "src/parser.mly"
               ( BLRR(_2))
# 728 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "src/parser.mly"
          ( BLR)
# 734 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "src/parser.mly"
          ( END)
# 740 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 124 "src/parser.mly"
               ( IN(_2,LL))
# 747 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 125 "src/parser.mly"
               ( IN(_2,LH))
# 754 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 126 "src/parser.mly"
               ( IN(_2,UL))
# 761 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 127 "src/parser.mly"
               ( IN(_2,UH))
# 768 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "src/parser.mly"
          ( NOP)
# 774 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 129 "src/parser.mly"
                ( OUT(_2,LL))
# 781 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 130 "src/parser.mly"
                ( OUT(_2,LH))
# 788 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 131 "src/parser.mly"
                ( OUT(_2,UL))
# 795 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 132 "src/parser.mly"
                ( OUT(_2,UH))
# 802 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 133 "src/parser.mly"
                                     ( SLAWI(_2,_4,_6))
# 811 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 134 "src/parser.mly"
                                    ( SRAWI(_2,_4,_6))
# 820 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "src/parser.mly"
    (
    let lex = (Parsing.symbol_end_pos ()).Lexing.pos_lnum in
         failwith ("syntax error at line " ^ string_of_int lex)
    )
# 829 "src/parser.ml"
               : 'order))
(* Entry exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.t list)
