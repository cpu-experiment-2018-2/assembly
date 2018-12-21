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

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t list
