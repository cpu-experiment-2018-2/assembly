external getint : float -> int = "getint"

open Syntax

let is32bit = ref false

let istext = ref true

let arg name = Array.exists (fun x -> x = name) Sys.argv

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
          if x >= 1 lsl 16 then [LIW (var, x)]
          else if x < 0 then [LIW (var, x)]
          else [LI (var, x)]
      | FLI (x, f) -> extend [LI (x, getint f)]
      | LIW (x, f) -> extend [LI (x, f)]
      | _ -> [x] )
      @ extend y

let lexbuf p =
  let p = Parser.exp Lexer.token p in
  p

let read_file name =
  let ic = open_in name in
  let p = lexbuf (Lexing.from_channel ic) in
  p

let libpath =
  match Sys.getenv_opt "CPU_LIB_PATH" with
  | Some x -> x
  | None -> failwith "Set CPU_LIB_PATH to the path of assembly/lib"

 let libs = List.map (fun x -> libpath ^ x) ["lib.s"; "trigonometric_kernels.s"] 
(* let libs = List.map (fun x -> libpath ^ x) [] *)


let _ =
  is32bit := arg "-32bit" ;
  istext := arg "-txt"

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
      (libs @ [main]) in
  let p = List.concat (fst libs) in
  let p =
    Label "HOGE"
    :: LI (0, 0)
    :: LI (1, 1)
    :: LI (2, 123800)
    :: BL "main" :: END :: p
  in
  if !is32bit then Bit32.f p !istext filename else Bit64.f p !istext filename
