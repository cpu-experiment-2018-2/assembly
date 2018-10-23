type token =
  | INT of (int)
  | FLOAT of (float)
  | COMMA
  | EOF
  | ADDI
  | INC
  | DEC
  | ADD
  | SUBI
  | SUB
  | SLAWI
  | SRAWI
  | MULI
  | MUL
  | DIVI
  | DIV
  | FADDI
  | FADD
  | FSUBI
  | FSUB
  | FMULI
  | FMUL
  | FDIVI
  | FDIV
  | MR
  | LI
  | LIL
  | LIS
  | FLI
  | STORE
  | LOAD
  | ITOF
  | FTOI
  | CMPDI
  | CMPD
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
# 63 "src/parser.ml"
let yytransl_const = [|
  259 (* COMMA *);
    0 (* EOF *);
  260 (* ADDI *);
  261 (* INC *);
  262 (* DEC *);
  263 (* ADD *);
  264 (* SUBI *);
  265 (* SUB *);
  266 (* SLAWI *);
  267 (* SRAWI *);
  268 (* MULI *);
  269 (* MUL *);
  270 (* DIVI *);
  271 (* DIV *);
  272 (* FADDI *);
  273 (* FADD *);
  274 (* FSUBI *);
  275 (* FSUB *);
  276 (* FMULI *);
  277 (* FMUL *);
  278 (* FDIVI *);
  279 (* FDIV *);
  280 (* MR *);
  281 (* LI *);
  282 (* LIL *);
  283 (* LIS *);
  284 (* FLI *);
  285 (* STORE *);
  286 (* LOAD *);
  287 (* ITOF *);
  288 (* FTOI *);
  289 (* CMPDI *);
  290 (* CMPD *);
  291 (* BL *);
  292 (* BEQ *);
  293 (* BLE *);
  294 (* BLR *);
  295 (* BLRR *);
  296 (* END *);
  297 (* INLL *);
  298 (* INLH *);
  299 (* INUL *);
  300 (* INUH *);
  301 (* OUTLL *);
  302 (* OUTLH *);
  303 (* OUTUL *);
  304 (* OUTUH *);
  305 (* JUMP *);
  306 (* COLON *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* FLOAT *);
  307 (* PERCENTINT *);
  308 (* IDENT *);
  309 (* ATATIDENT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\004\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\001\000\001\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\004\000\004\000\004\000\004\000\006\000\006\000\004\000\
\004\000\004\000\002\000\004\000\002\000\004\000\002\000\002\000\
\002\000\002\000\002\000\001\000\001\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\006\000\006\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\048\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\000\000\037\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\049\000\
\000\000\005\000\004\000\000\000\027\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\031\000\
\032\000\035\000\038\000\039\000\040\000\041\000\042\000\043\000\
\044\000\045\000\034\000\003\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\028\000\020\000\021\000\018\000\
\019\000\000\000\000\000\030\000\026\000\024\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
\010\000\008\000\012\000\046\000\047\000\007\000\011\000\009\000\
\013\000\014\000\016\000\015\000\017\000\023\000\022\000"

let yydgoto = "\002\000\
\048\000\049\000\050\000\052\000"

let yysindex = "\255\255\
\001\000\000\000\000\000\000\000\233\254\233\254\233\254\233\254\
\233\254\233\254\233\254\233\254\233\254\233\254\233\254\233\254\
\233\254\233\254\233\254\233\254\233\254\233\254\233\254\233\254\
\233\254\233\254\233\254\233\254\233\254\233\254\233\254\234\254\
\235\254\237\254\000\000\233\254\000\000\233\254\233\254\233\254\
\233\254\233\254\233\254\233\254\233\254\246\254\249\254\000\000\
\001\000\000\000\000\000\026\255\000\000\000\000\041\255\042\255\
\043\255\044\255\045\255\046\255\047\255\048\255\049\255\050\255\
\051\255\052\255\053\255\054\255\055\255\056\255\057\255\058\255\
\059\255\060\255\061\255\062\255\063\255\064\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\233\254\233\254\233\254\
\233\254\233\254\233\254\233\254\233\254\233\254\233\254\233\254\
\233\254\233\254\233\254\233\254\067\255\017\255\069\255\070\255\
\233\254\233\254\233\254\233\254\072\255\233\254\068\255\071\255\
\073\255\074\255\075\255\076\255\077\255\078\255\079\255\080\255\
\081\255\082\255\083\255\084\255\000\000\000\000\000\000\000\000\
\000\000\085\255\086\255\000\000\000\000\000\000\000\000\104\255\
\233\254\105\255\233\254\106\255\107\255\112\255\233\254\114\255\
\233\254\233\254\233\254\233\254\233\254\115\255\116\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\026\000\000\000\000\000\252\255"

let yytablesize = 309
let yytable = "\001\000\
\004\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\051\000\094\000\079\000\080\000\082\000\
\081\000\083\000\084\000\085\000\086\000\087\000\088\000\089\000\
\090\000\091\000\092\000\095\000\096\000\097\000\098\000\099\000\
\100\000\101\000\102\000\103\000\104\000\105\000\106\000\107\000\
\108\000\109\000\110\000\111\000\112\000\113\000\114\000\115\000\
\116\000\117\000\118\000\134\000\135\000\136\000\144\000\137\000\
\142\000\145\000\093\000\146\000\147\000\148\000\149\000\150\000\
\151\000\152\000\153\000\154\000\155\000\156\000\157\000\158\000\
\159\000\119\000\120\000\121\000\122\000\123\000\124\000\125\000\
\126\000\127\000\128\000\129\000\130\000\131\000\132\000\133\000\
\160\000\162\000\164\000\165\000\138\000\139\000\140\000\141\000\
\166\000\143\000\168\000\174\000\175\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\161\000\000\000\163\000\000\000\
\000\000\000\000\167\000\000\000\169\000\170\000\171\000\172\000\
\173\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\003\000\000\000\000\000\000\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\012\000\013\000\014\000\015\000\016\000\
\000\000\017\000\000\000\018\000\000\000\019\000\000\000\020\000\
\021\000\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\046\000\000\000\000\000\047\000"

let yycheck = "\001\000\
\000\000\006\000\007\000\008\000\009\000\010\000\011\000\012\000\
\013\000\014\000\015\000\016\000\017\000\018\000\019\000\020\000\
\021\000\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\031\000\051\001\003\001\052\001\052\001\036\000\
\052\001\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\052\001\050\001\003\001\003\001\003\001\003\001\003\001\
\003\001\003\001\003\001\003\001\003\001\003\001\003\001\003\001\
\003\001\003\001\003\001\003\001\003\001\003\001\003\001\003\001\
\003\001\003\001\003\001\001\001\052\001\001\001\003\001\002\001\
\001\001\003\001\049\000\003\001\003\001\003\001\003\001\003\001\
\003\001\003\001\003\001\003\001\003\001\003\001\003\001\003\001\
\003\001\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\106\000\107\000\108\000\
\001\001\001\001\001\001\001\001\113\000\114\000\115\000\116\000\
\001\001\118\000\001\001\001\001\001\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\145\000\255\255\147\000\255\255\
\255\255\255\255\151\000\255\255\153\000\154\000\155\000\156\000\
\157\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\255\255\255\255\255\255\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\255\255\017\001\255\255\019\001\255\255\021\001\255\255\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\041\001\042\001\043\001\044\001\045\001\046\001\047\001\
\048\001\049\001\255\255\255\255\052\001"

let yynames_const = "\
  COMMA\000\
  EOF\000\
  ADDI\000\
  INC\000\
  DEC\000\
  ADD\000\
  SUBI\000\
  SUB\000\
  SLAWI\000\
  SRAWI\000\
  MULI\000\
  MUL\000\
  DIVI\000\
  DIV\000\
  FADDI\000\
  FADD\000\
  FSUBI\000\
  FSUB\000\
  FMULI\000\
  FMUL\000\
  FDIVI\000\
  FDIV\000\
  MR\000\
  LI\000\
  LIL\000\
  LIS\000\
  FLI\000\
  STORE\000\
  LOAD\000\
  ITOF\000\
  FTOI\000\
  CMPDI\000\
  CMPD\000\
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
# 64 "src/parser.mly"
                ( _1::_2 )
# 371 "src/parser.ml"
               : Syntax.t list))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "src/parser.mly"
          ([])
# 377 "src/parser.ml"
               : Syntax.t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 67 "src/parser.mly"
                  (_1)
# 384 "src/parser.ml"
               : 'label))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 69 "src/parser.mly"
                 (_1)
# 391 "src/parser.ml"
               : 'reg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'label) in
    Obj.repr(
# 71 "src/parser.mly"
            ( 
        let t =  _1 in
        if String.length t < 2 then Label(t) else 
        (
            let t = String.sub t 0 2 in
            if t = "@@" then LocalLabel(_1) else Label(_1)
        )
        )
# 405 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 79 "src/parser.mly"
                                   ( ADDI(_2,_4,_6) )
# 414 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 80 "src/parser.mly"
                                   ( MULI(_2,_4,_6) )
# 423 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 81 "src/parser.mly"
                                   ( SUBI(_2,_4,_6) )
# 432 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 82 "src/parser.mly"
                                   ( DIVI(_2,_4,_6) )
# 441 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 83 "src/parser.mly"
                                  ( ADD(_2,_4,_6) )
# 450 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 84 "src/parser.mly"
                                  ( MUL(_2,_4,_6) )
# 459 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 85 "src/parser.mly"
                                  ( SUB(_2,_4,_6) )
# 468 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 86 "src/parser.mly"
                                  ( DIV(_2,_4,_6) )
# 477 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 87 "src/parser.mly"
                                   ( FADD(_2,_4,_6) )
# 486 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 88 "src/parser.mly"
                                   ( FMUL(_2,_4,_6) )
# 495 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 89 "src/parser.mly"
                                   ( FSUB(_2,_4,_6) )
# 504 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 90 "src/parser.mly"
                                   ( FDIV(_2,_4,_6) )
# 513 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "src/parser.mly"
                        ( LIS(_2,_4))
# 521 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 92 "src/parser.mly"
                          ( FLI(_2,_4))
# 529 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 93 "src/parser.mly"
                       ( LI(_2,_4))
# 537 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 94 "src/parser.mly"
                          ( LIL(_2,_4))
# 545 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 95 "src/parser.mly"
                                   ( LOAD(_2,_4,_6))
# 554 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 96 "src/parser.mly"
                                    ( STORE(_2,_4,_6))
# 563 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 97 "src/parser.mly"
                          ( CMPDI(_2,_4))
# 571 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 98 "src/parser.mly"
                         ( CMPD(_2,_4))
# 579 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 99 "src/parser.mly"
                         ( CMPD(_2,_4))
# 587 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 100 "src/parser.mly"
              ( ADDI(_2,_2,1))
# 594 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 101 "src/parser.mly"
                       ( ADDI(_2,_4,0))
# 602 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 102 "src/parser.mly"
              ( SUBI(_2,_2,1))
# 609 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 103 "src/parser.mly"
                         ( CMPD(_2,_4))
# 617 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "src/parser.mly"
                ( BEQ(_2))
# 624 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "src/parser.mly"
                ( BLE(_2))
# 631 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 106 "src/parser.mly"
               ( BL(_2))
# 638 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 107 "src/parser.mly"
                 (JUMP(_2))
# 645 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 108 "src/parser.mly"
               ( BLRR(_2))
# 652 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "src/parser.mly"
          ( BLR)
# 658 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "src/parser.mly"
          ( END)
# 664 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 111 "src/parser.mly"
               ( IN(_2,LL))
# 671 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 112 "src/parser.mly"
               ( IN(_2,LH))
# 678 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 113 "src/parser.mly"
               ( IN(_2,UL))
# 685 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 114 "src/parser.mly"
               ( IN(_2,UH))
# 692 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 115 "src/parser.mly"
                ( OUT(_2,LL))
# 699 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 116 "src/parser.mly"
                ( OUT(_2,LH))
# 706 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 117 "src/parser.mly"
                ( OUT(_2,UL))
# 713 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 118 "src/parser.mly"
                ( OUT(_2,UH))
# 720 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 119 "src/parser.mly"
                                     ( SLAWI(_2,_4,_6))
# 729 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'reg) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'reg) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 120 "src/parser.mly"
                                    ( SRAWI(_2,_4,_6))
# 738 "src/parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "src/parser.mly"
    (
    let lex = (Parsing.symbol_end_pos ()).Lexing.pos_lnum in
         failwith ("syntax error at line " ^ string_of_int lex)
    )
# 747 "src/parser.ml"
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
