(* ラベルごとに分ける *)
open Syntax
let is_br = function
  | BEQ _ | BLE _ | BL _ | BLRR _ | BLT _ | BNE _ | BGE _ | BGT _ | JUMP _
  | BLR _  ->
      true
  | _ -> false

let is_out_in = function
    | OUT _ | IN _ -> true
    | _ -> false
let is_side_effect = function
    | STORE _ | LOAD _ -> true
    | _ -> false
let is_uncond_br = function JUMP _ | BLR _ -> true | _ -> false

let is_64bit = function LIW _ -> true | _ -> false

let get_until_label x =
  let rec sub z acc =
    match z with
    | Label x :: y | LocalLabel x :: y -> (acc, z)
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

let safe = false

let rec g (label, z) =
  let left = Array.make (List.length z * 9) NOP in
  let right = Array.make (List.length z * 9) NOP in
  let orders = Array.of_list z in
  let _ = Printf.printf "label %s nyaan %d" label (List.length z) in
  let can_time = Array.make 34 0 in
  let used_time = Array.make 34 0 in
  let time = ref 0 in
  let min_time = ref 0 in
  let dummy = 33 in
  let _ =
    for i = 0 to Array.length orders - 1 do
      let o = orders.(i) in
      (* let _ = Printf.printf "%d %s \n" i (Syntax.show o) in *)
      let gen, start_time, latency, ops =
        match o with
        | ADDI (a, b, c) | SUBI (a, b, c) | SRAWI (a, b, c) | SLAWI (a, b, c)
          ->
            (a, can_time.(b), 2, [b])
        | FADD (a, b, c) | FSUB (a, b, c) | FMUL (a, b, c) ->
            (a, max can_time.(b) can_time.(c), 4, [b; c])
        | FDIV (a, b, c) -> (a, max can_time.(b) can_time.(c), 6, [b; c])
        | ADD (a, b, c) | SUB (a, b, c) | AND (a, b, c) | XOR (a, b, c) ->
            (a, max can_time.(b) can_time.(c), 2, [b; c])
        | LI (a, b) | LIW (a, b) -> (a, 0, 2, [])
        | FTOI (a, b) | ITOF (a, b) | FFLOOR (a, b) ->
            (a, can_time.(b), 3, [b])
        | FSQRT (a, b) ->
            (a, can_time.(b), 4, [b])
        | STORE (a, b, c) -> (dummy, max can_time.(b) can_time.(a), 0, [a;b])
        | LOAD (a, b, c) -> (a, can_time.(b), 4, [b])
        | CMPDI (a, b) -> (dummy, can_time.(a), 0, [a])
        | CMPD (a, b) -> (dummy, max can_time.(a) can_time.(b), 0, [a; b])
        | CMPF (a, b) -> (dummy, max can_time.(a) can_time.(b), 0, [a; b])
        | BLRR a -> (dummy, !time, 0, [a])
        | BL _ 
        ->
            (3, !time, 0, [])

        | BEQ _ | BLE _ | BLT _ | BNE _ | BGE _ | BGT _ | JUMP _ | BLR _
          ->
            (dummy, !time, 0, [])
        | END -> (dummy, !time, 0, [])
        | IN (a, _) -> (a, !time, 3, [])
        | OUT (a, _) -> (dummy, !time, 0, [a])
        | _ -> failwith (Syntax.show o)
      in
      let start_time = if safe then !time else max (!min_time) (max used_time.(gen) start_time) in
      let setted = ref false in
      let _ =
        Printf.printf "start search %s %d\n" (Syntax.show o) start_time
      in
      for j = start_time to start_time + 20 do
        if not !setted then
          let _ =
            match (o, left.(j), right.(j)) with
            | LIW (a, b), NOP, NOP ->
                left.(j) <- o ;
                right.(j) <- o ;
                setted := true
            | x, NOP, NOP ->
                left.(j) <- x ;
                setted := true
            | x, OUT _, NOP -> ()
            | x, IN _, NOP -> ()
            (* | LOAD (a, b, c), LOAD (d, e, f), NOP -> *)
            (*     if b = e && c = f + 1 then ( *)
            (*       right.(j) <- o ; *)
            (*       setted := true ) *)
            (*     else () *)
            (* | STORE (a, b, c), STORE (d, e, f), NOP -> *)
            (*     if b = e && c = f + 1 then ( *)
            (*       right.(j) <- o ; *)
            (*       setted := true ) *)
            (*     else () *)
            | x, y, NOP
              when (not (is_br x)) && (not (is_br y)) && not (is_64bit x) && not (is_out_in x)->
                (*   if (match o with | LOAD _ | STORE _ -> true | _ -> false) then *)
                (*       ( *)
                (* right.(j) <- left.(j); *)
                (* left.(j) <- x ; *)
                (*       )else ( *)
                right.(j) <- x ;
                      (* ); *)
                setted := true
            | _ -> ()
          in
          if !setted then (
            can_time.(gen) <- j + latency + 1 ;
            (* if safe then time := !time + 8  else time := max !time (j + latency + 1) ; *)
            (* if safe then time := j+ latency + 1  else time := max !time (j + latency + 1) ; *)
            if safe then time := j + latency + 1  else time := max !time (j + latency + 1) ;
            if is_br o then (
                min_time := j + latency + 1;
                for i = 0 to 33 do 
                    can_time.(i) <- j + latency + 1;
                    used_time.(i) <- j + latency + 1;
                done
            ) else if is_side_effect o then (
                 min_time := j ;
            ) else ();
            List.iter
              (fun x -> used_time.(x) <- max j used_time.(x))
              ops )
          else ()
        else ()
      done
    done
  in
  (* let _ = Printf.printf "time %d :\n" !time in  *)
  let last = ref !time in
  let _ =
    for i = 0 to Array.length left - 1 do
      if left.(i) <> NOP then last := i else ()
    done ;
    for i = 0 to 33 do
      last := max !last (can_time.(i) - 1)
    done
  in
  let order = Array.make (!last + 1) (NOP, NOP) in
  let _ =
    for i = 0 to !last do
      let _ = order.(i) <- (left.(i), right.(i)) in
      Printf.printf "%d %s %s\n" i
        (Syntax.show left.(i))
        (Syntax.show right.(i))
    done
  in
  order

let rec f x =
  let y = Eliminate_dead_code.f x in
  let y =
    List.map
      (fun (y, x) ->
        if List.length x > 0 then
          let arr = g (y, x) in
          (y, arr)
        else (y, [||]) )
      y
  in
  y
