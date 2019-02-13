open Syntax
let use = Hashtbl.create 123456
let get_until_label x =
  let rec sub z acc =
    match z with
    | Label x :: y | LocalLabel x :: y -> (acc, z)
    | x :: y -> sub y (x :: acc)
    | [] -> (acc, [])
  in
  let hoge, reg = sub x [] in
  (List.rev hoge, reg)

let get_until_normal_label x =
  let rec sub z acc =
    match z with
    | Label x :: y -> (acc, z)
    | x :: y -> sub y (x :: acc)
    | [] -> (acc, [])
  in
  let hoge, reg = sub x [] in
  (List.rev hoge, reg)

let rec h x =
  match x with
  | Label x :: y | LocalLabel x :: y ->
      let z, rest = get_until_label y in
      (x, z) :: h rest
  | _ -> []


let rec h2 x =
  match x with
  | Label x :: y -> 
      let z, rest = get_until_normal_label y in
      (x, z) :: h2 rest
  | _ -> []

let rec f x = 
    let x1 = h2 x  in
    let _ = List.iter (fun (x,y) -> 
        Printf.printf "label %s\n" x ;
        List.iter (fun z -> 
        Printf.printf "%s\n" (Syntax.show z)
        ) y) in
    let rec g cur = 
        print_string cur;
        print_string "\n";
        if Hashtbl.mem use cur then ()
        else
            (
                let _ = Hashtbl.add use cur () in
                let v = List.assoc_opt cur x1 in
                match v with
                | Some(v) -> 
                        (
                List.iter (fun l -> 
                    match l with 
                    | BEQ s | BLE s | BL s | BLT s | BNE s | BGE s | BGT s | JUMP s -> 
                            g s
                    | LocalLabel s -> Hashtbl.add use s () 
                    | Label s -> failwith "hen"
                    | _ -> ()
                    ) v
                        )
                |None -> ()
            )
    in 
    let _ = g "HOGE" in
    let x = h x in
    let y = List.filter (fun (a,b) -> Hashtbl.mem use a) x in
    y
    
