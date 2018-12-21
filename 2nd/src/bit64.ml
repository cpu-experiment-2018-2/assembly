open Syntax

external getint : float -> int = "getint"

let to_bin p =
  let conv x =
    let t63_56 = "0b" ^ String.sub x 0 8 in
    let t55_48 = "0b" ^ String.sub x 8 8 in
    let t47_40 = "0b" ^ String.sub x 16 8 in
    let t39_32 = "0b" ^ String.sub x 24 8 in
    let t31_24 = "0b" ^ String.sub x 32 8 in
    let t23_16 = "0b" ^ String.sub x 40 8 in
    let t15_8 = "0b" ^ String.sub x 48 8 in
    let t7_0 = "0b" ^ String.sub x 56 8 in
    List.map
      (fun x -> x |> int_of_string |> Char.chr |> Bytes.make 1)
      [t63_56; t55_48; t47_40; t39_32; t31_24; t23_16; t15_8; t7_0]
  in
  Bytes.concat Bytes.empty (List.concat (List.map conv p))

let rec extend x =
  match x with
  | [] -> []
  | x :: y ->
      ( match x with
      | LI (var, x) ->
          if x >= 1 lsl 16 then [LIW (var, x)]
          else if x < 0 then [LIW (var, x)]
          else [LI (var, x)]
      | FLI (x, f) -> extend [LI (x, getint f)]
      | LIW (x, f) -> extend [LI (x, f)]
      | _ -> [x] )
      @ extend y

let opt p = RmSucJump.f p

let make_env' (env, counter) expr =
  let s, arr = expr in
  ((s, counter) :: env, counter + Array.length arr)

let make_env p = List.fold_left make_env' ([], 0) p

let encode' env (label, ar) =
  let lis = Array.to_list ar in
  let _ = Printf.printf "neko %s:\n" label in
  let lis =
    List.map
      (fun (x, y) ->
        let [x] = Encode.encode env x in
        let [y] = Encode.encode env y in
        let _ = Printf.printf "neko %s %s\n" x y in
        if String.length x = 32 then x ^ y else x )
      lis
  in
  lis

let encode p filename =
  let p = p |> extend |> opt in
  let oc = open_out (filename ^ ".debug") in
  let p = Vliw.f p in
  let counter = ref 0 in
  let _ =
    List.iter
      (fun (label, p) ->
        let _ = Printf.fprintf oc "%s:\n" label in
        Array.iter
          (fun (x, y) ->
            Printf.fprintf oc "%d: 1: %s , 2: %s\n" !counter (Syntax.show x)
              (Syntax.show y) ;
            counter := !counter + 1 )
          p )
      p
  in
  let env = fst (make_env p) in
  let p = List.concat (List.map (encode' env) p) in
  p

let rec f p istext filename =
  let p = encode p filename in
  if istext then
    let oname = filename ^ ".txt" in
    let oc = open_out oname in
    let _ = List.iter (Printf.fprintf oc "%s\n") p in
    Printf.printf "outputed to %s\n" oname
  else
    let oname = filename ^ ".oo" in
    let oc = open_out oname in
    let _ = output_bytes oc (to_bin p) in
    Printf.printf "outputed to %s\n" oname
