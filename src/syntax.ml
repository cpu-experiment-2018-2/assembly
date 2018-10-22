type pos = LL | LH | UL | UH [@@deriving show]

type t =
  | Label of string
  | LocalLabel of string
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
  | SRAWI of int * int * int
  | SLAWI of int * int * int
  | LI of int * int
  | FLI of int * float
  | LIS of int * int
  | LIL of int * string
  | STORE of int * int * int
  | LOAD of int * int * int
  | CMPDI of int * int
  | CMPD of int * int
  | BEQ of string
  | BLE of string
  | JUMP of string
  | BL of string
  | IN of int * pos
  | OUT of int * pos
  | BLR
  | END
[@@deriving show]

let apply f s =
  match s with
  | Label s -> Label (f s)
  | LocalLabel s -> LocalLabel (f s)
  | LIL (t,s) -> LIL (t, f s)
  | BEQ s -> BEQ (f s)
  | BLE s -> BLE (f s)
  | BL s -> BL (f s)
  | JUMP s -> JUMP (f s)
  | _ -> s

let change prefix e =
  let rec f t = match t with LocalLabel x -> [x] | _ -> [] in
  let globals = List.concat (List.map f e) in
  let change x =
    if not (List.exists (fun y -> y = x) globals) then x else prefix ^ x
  in
  List.map (apply change) e
