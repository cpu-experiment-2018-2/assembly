open Syntax

let get_pos_in = function
  | LL -> "000"
  | LH -> "001"
  | UL -> "010"
  | UH -> "011"

let get_pos_out = function
  | LL -> "100"
  | LH -> "101"
  | UL -> "110"
  | UH -> "111"

let opcode e =
  match e with
  | Label _ -> failwith "label doesn't appear in bin"
  | LocalLabel _ -> failwith "LocalLabel doesn't appear in bin"
  | ADDI _ -> "000000"
  | SUBI _ -> "000001"
  | ADD _ -> "000010"
  | SUB _ -> "000011"
  | FADD _ -> "001000"
  | FSUB _ -> "001001"
  | FMUL _ -> "001010"
  | FDIV _ -> "001011"
  | SRAWI _ -> "000100"
  | SLAWI _ -> "000101"
  | LOAD _ -> "010000"
  | STORE _ -> "010001"
  | JUMP _ -> "011000"
  | BEQ _ -> "100000"
  | BLE _ -> "100001"
  | BLT _ -> "100010"
  | AND _ -> "010000"
  | OR _ -> "010001"
  | IN (_, x) -> "101" ^ get_pos_in x
  | OUT (_, x) -> "101" ^ get_pos_out x
  | END -> "111000"
  | BLR -> "011001"
  | BL _ -> "011010"
  | BLRR _ -> "011011"
  | CMPF _ -> "100100"
  | CMPD _ -> "100011"
  | CMPDI _ -> "100101"
  | LI _ -> "010010"
  | LIS _ -> "010011"
  | NOP -> "111000"

let binary_encode keta number =
  let str = Bytes.make keta '0' in
  let _ =
    for i = 0 to keta - 1 do
      if (number lsr i) land 1 = 1 then Bytes.set str (keta - 1 - i) '1'
      else Bytes.set str (keta - 1 - i) '0'
    done
  in
  Bytes.to_string str

let rec encode env e =
  match e with
  | Label s -> []
  | LocalLabel s -> []
  | LIL (t, label) ->
      encode env
        ( match List.find_opt (fun (x, y) -> label = x) env with
        | Some (x, y) -> LI (t, y)
        | None -> failwith label )
  | _ ->
      let op = opcode e in
      let t =
        binary_encode 26
          ( match e with
          | ADDI (t, s, d) | SUBI (t, s, d) -> (t lsl 21) lor (s lsl 16) lor d
          | ADD (t, a, b)
           |SUB (t, a, b)
           |FADD (t, a, b)
           |FSUB (t, a, b)
           |FMUL (t, a, b)
           |FDIV (t, a, b)
           |AND (t, a, b)
           |OR (t, a, b) ->
              (t lsl 21) lor (a lsl 16) lor (b lsl 11)
          | JUMP label | BEQ label | BLE label | BL label | BLT label -> (
            match List.find_opt (fun (x, y) -> label = x) env with
            | Some (x, y) -> y
            | None -> failwith label )
          | LOAD (t, a, d) | STORE (t, a, d) | SLAWI (t, a, d) | SRAWI (t, a, d) ->
              (t lsl 21) lor (a lsl 16) lor d
          | LI (t, d) -> (t lsl 21) lor d
          | CMPDI (t, d) -> (t lsl 21) lor d
          | LIS (t, d) -> (t lsl 21) lor d
          | CMPD (a, b) | CMPF (a, b) -> (a lsl 21) lor (b lsl 16)
          | BLRR a | IN (a, _) | OUT (a, _) -> a lsl 21
          | Label _ -> failwith "label is unreachble"
          | BLR _ -> 0
          | END _ -> 0
          | _ -> failwith "yet implemented" )
      in
      [op ^ t]

let make_env (env, counter) expr =
  match expr with
  | Label s -> ((s, counter) :: env, counter)
  | LocalLabel s -> ((s, counter) :: env, counter)
  | _ -> (env, counter + 1)

let f exp_list =
  let env, counter = List.fold_left make_env ([], 0) exp_list in
  List.concat (List.map (encode env) exp_list)
