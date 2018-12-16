external getint : float -> int = "getint"

open Syntax

let to_bin p =
  let conv x =
    let t31_24 = "0b" ^ String.sub x 0 8 in
    let t23_16 = "0b" ^ String.sub x 8 8 in
    let t15_8 = "0b" ^ String.sub x 16 8 in
    let t7_0 = "0b" ^ String.sub x 24 8 in
    List.map
      (fun x -> x |> int_of_string |> Char.chr |> Bytes.make 1)
      [t31_24; t23_16; t15_8; t7_0]
  in
  Bytes.concat Bytes.empty (List.concat (List.map conv p))

let arg name argv = Array.exists (fun x -> x = name) argv

let _ =
  Printf.printf
    "usage: ./main.native filename [-option]\n\
     \t-txt : convert to binary string outputed to filename.txt \n\
     \totherwise : convert to binary outputed to filename.oo\n"

let rec extend x =
  match x with
  | [] -> []
  | x :: y ->
      ( match x with
      | LI (var, x) ->
          if x >= 1 lsl 16 then
            [LI (var, x land ((1 lsl 16) - 1)); LIS (var, x lsr 16)]
          else if x < 0 then
            [ LI (var, x land ((1 lsl 16) - 1))
            ; LIS (var, (x lsr 16) land ((1 lsl 16) - 1)) ]
          else [LI (var, x)]
      | FLI (x, f) -> extend [LI (x, getint f)]
      | LIW (x, f) -> extend [LI (x, f)]
      | LocalLabel(s) -> [Label(s)]
      (* | FFLOOR(t,s) -> [ FTOI(t,s); ITOF(t,t) ] *)
      | _ -> [x] )
      @ extend y

(* let rec extend x = *)
(*   match x with *)
(*   | [] -> [] *)
(*   | x :: y -> *)
(*       ( match x with *)
(*       | LI (var, x) -> *)
(*           if x >= 1 lsl 16 then *)
(*             [LI(var,x land ((1 lsr 16)-1));LIS (var, x lsr 16);] *)
(*           else if x < 0 then *)
(*             [LI(var,x land ((1 lsr 16)-1));LIS (var, x lsr 16);] *)
(*           else [LI (var, x)] *)
(*       | FLI (x, f) -> extend [LI (x, getint f)] *)
(*       | LIW (x, f) -> extend [LI (x, f)] *)
(*       | _ -> [x] ) *)
(*       @ extend y *)

(* let rec extend x = *)
(*   match x with *)
(*   | [] -> [] *)
(*   | x :: y -> *)
(*       ( match x with *)
(*       | LI (var, x) -> *)
(*           if x >= 1 lsl 16 then *)
(*             [LIW (var, x)] *)
(*           else if x < 0 then *)
(*             [LIW (var, x)] *)
(*           else [LI (var, x)] *)
(*       | FLI (x, f) -> extend [LI (x, getint f)] *)
(*       | LIW (x, f) -> extend [LI (x, f)] *)
(*       | _ -> [x] ) *)
(*       @ extend y *)
(*  *)


let lexbuf p =
  let p = Parser.exp Lexer.token p in
  p
let opt p = 
    RmSucJump.f p
let encode p filename =
  let p = p |> extend |> opt in
  let oc = open_out (filename ^ ".debug") in
  let counter = ref 0 in
  let _ =
    List.iter
      (fun x ->
          let _ = 
        match x with
        | LocalLabel x | Label x -> Printf.fprintf oc "%s: \n" x
        | _ -> counter := !counter + 1  in 
            Printf.fprintf oc "%d: %s\n" !counter (Syntax.show x) ;
            )
      p
  in
  Encode.f p

let read_file name =
  let ic = open_in name in
  let p = lexbuf (Lexing.from_channel ic) in
  p

let libpath =
  match Sys.getenv_opt "CPU_LIB_PATH" with
  | Some x -> x
  | None -> failwith "Set CPU_LIB_PATH to the path of assembly/lib"

let libs =
  List.map
    (fun x -> libpath ^ x)
    [ "lib.s"
    ; "trigonometric_kernels.s"
    ;  ]

let _ =
  let filename = Sys.argv.(1) in
  let main = read_file filename in
  let _ = Printf.printf "Warning: Loading a libray\n" in
  let libs =
    List.map
      (fun x ->
        Printf.printf "linking %s\n" x ;
        read_file x )
      libs
  in
  let libs =
    List.fold_left
      (fun (acc, counter) p ->
        (change (string_of_int counter) p :: acc, counter + 1) )
      ([], 0)
      (libs @ [main])
  in
  let p = List.concat (fst libs) in
  (* 暇なとき実装する *)
  let p = LI (0,0)::LI(1,1)::LI(2,1000000)::(BL "main" ):: END :: p in
  let p = encode p filename in
  if arg "-txt" Sys.argv then
    let oname = filename ^ ".txt" in
    let oc = open_out oname in
    let _ = List.iter (Printf.fprintf oc "%s\n") p in
    Printf.printf "outputed to %s\n" oname
  else
    let oname = filename ^ ".oo" in
    let oc = open_out oname in
    let _ = output_bytes oc (to_bin p) in
    Printf.printf "outputed to %s\n" oname
