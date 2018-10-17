type pos = LL | LH | UL | UH [@@deriving show]

type t =
  | Label of string
  | ADDI of int * int * int
  | MULI of int * int * int
  | DIVI of int * int * int
  | SUBI of int * int * int
  | ADD of int * int * int
  | MUL of int * int * int
  | DIV of int * int * int
  | SUB of int * int * int
  | FADD of int * int * int
  | FMUL of int * int * int
  | FDIV of int * int * int
  | FSUB of int * int * int
  | AND of int * int * int
  | OR of int * int * int
  | LI of int * int
  | LIS of int * int
  | STORE of int * int * int
  | LOAD of int * int * int
  | CMPDI of int * int
  | CMPD of int * int
  | BEQ of string
  | BLE of string
  | JUMP of string
  | FTOI of int * int
  | ITOF of int * int
  | IN of int * pos
  | OUT of int * pos
  | BLR
  | BL of string
  | END
[@@deriving show]
