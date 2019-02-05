(* ラベルごとに分ける *)
open Syntax

let is_br = function
  | BEQ _ | BLE _ | BL _ | BLRR _ | BLT _ | BNE _ | BGE _ | BGT _ | JUMP _
   |BLR _ ->
      true
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

let safe = true
let get_info o = 
     match o with
        | ADDI (a, b, c) | SUBI (a, b, c) | SRAWI (a, b, c) | SLAWI (a, b, c)
          ->
            (Some a,  2, [b])
        | FADD (a, b, c) | FSUB (a, b, c) | FMUL (a, b, c) ->
            (Some a, 4, [b; c])
        | FDIV (a, b, c) -> (Some a, 6, [b; c])
        | ADD (a, b, c) | SUB (a, b, c) | AND (a, b, c) | XOR (a, b, c) ->
            (Some a, 2, [b; c])
        | LI (a, b) | LIW (a, b) -> (Some a, 2, [])
        | FTOI (a, b) | ITOF (a, b) | FSQRT (a, b) | FFLOOR (a, b) ->
            (Some a, 3, [b])
        | STORE (a, b, c) -> (None, max can_time.(b) can_time.(a), 0, [a;b])
        | LOAD (a, b, c) -> (Some a,  can_time.(a), 4, [b])
        | CMPDI (a, b) -> (None, 0, [a])
        | CMPD (a, b) -> (None,  0, [a; b])
        | CMPF (a, b) -> (None,  0, [a; b])
        | BLRR a -> (None, 2, [a])
        | BL _ 
        ->
            (None, 0, [3])

        | BEQ _ | BLE _ | BLT _ | BNE _ | BGE _ | BGT _ | JUMP _ | BLR _
          ->
            (None, 1, [])
        | END -> (None, 0, [])
        | IN (a, _) -> (a, 2, [a])
        | OUT (a, _) -> (dummy, 0, [a])
        | _ -> failwith (Syntax.show o)


let rec g (label, z) =
  let left = Array.make (List.length z * 9) NOP in
  let right = Array.make (List.length z * 9) NOP in
  let orders = Array.of_list z in
  let _ = Printf.printf "label %s nyaan %d" label (List.length z) in
  let def_time = Array.make 34 0 in
  let last_use_time = Array.make 34 0 in
  let time = ref 0 in
  let min_time = ref 0 in
  let dummy = 33 in
  let _ =
    for i = 0 to Array.length orders - 1 do
      let o = orders.(i) in
      let start_time = if safe then !time else max (!min_time) (max can_time.(gen) start_time) in
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
            | LOAD (a, b, c), LOAD (d, e, f), NOP ->
                if b = e && c = f + 1 then (
                  right.(j) <- o ;
                  setted := true )
                else ()
            | STORE (a, b, c), STORE (d, e, f), NOP ->
                if b = e && c = f + 1 then (
                  right.(j) <- o ;
                  setted := true )
                else ()
            | x, y, NOP
              when (not (is_br x)) && (not (is_br y)) && not (is_64bit x) ->
                right.(j) <- x ;
                setted := true
            | _ -> ()
          in
          if !setted then (
            can_time.(gen) <- start_time + latency + 1 ;
            if safe then time := !time + 1 else time := max !time (start_time + latency) ;
            if is_br o then (min_time := start_time + 1) else ();
            List.iter
              (fun x -> can_time.(x) <- max start_time can_time.(x))
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
  let y = h x in
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
