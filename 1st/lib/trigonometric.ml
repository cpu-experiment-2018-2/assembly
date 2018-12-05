let rec sin x = 
    let flag = if x >= 0.0 then 1.0 else -1.0 in
    let x = flag *. x in
    let x = reduction x in
    let pi = 3.141592 in
    let pi4 = 0.7853981 in
    let pi2 = pi4 *. 2.0 in
    let flag = if x >= pi then -1.0 *. flag else flag in
    let x = if x >= pi then (x-.pi)  else x in
    let x = if x >= pi2 then pi -. x else x in
    (* let _ = print_float x in *)
    flag *. (if x <= pi4 then kernel_sin x else kernel_cos (pi2-.x))  in

let rec cos x = 
    let flag = 1.0 in
    let x = if x>= 0. then x else -1.0 *. x  in
    let x = reduction x in
    let pi = 3.141592 in
    let pi4 = 0.7853981 in
    let pi2 = pi4 *. 2.0 in

    let flag = if x >= pi then -1.0 *. flag else flag in
    let x = if x >= pi then (x-.pi)  else x in

    let flag = if x >= pi2 then -1.0 *. flag else flag in
    let x = if x >= pi2 then (pi-.x)  else x in
    flag *. (if x <= pi4 then kernel_cos x else kernel_sin (pi2-.x)) 
in
let rec atan x = 
    let flag = if x >= 0.0 then 1.0 else -1.0 in
    let x = flag *. x in
    let pi4 = 0.7853981 in
    let pi2 = pi4 *. 2.0 in
    flag *. (
    if x <= 0.4375 then
        kernel_atan(x)
    else if x <= 1.0 then
        (
            pi4 -. kernel_atan ( ( 1.0 -. x ) /. ( x +. 1.0))
        )
    else if x <= 2.41421356 then
        (
            pi4 +. kernel_atan( ( x -.1.0) /.( x +. 1.0))
        )
    else 
        (
            pi2 -. kernel_atan(1.0 /. x)
        )
    )
in
()
(* print_float(sin(0.392699081699)); *)
(* print_float(sin(1.1780972451)); *)
(* print_float(sin(1.96349540849)); *)
(* print_float(sin(2.74889357189)); *)
(* print_float(sin(3.53429173529)); *)
(* print_float(sin(4.31968989869)); *)
(* print_float(sin(5.10508806208)); *)
(* print_float(sin(5.89048622548)) *)

(* (* 0.382683432365 *) *)
(* (* 0.923879532511 *) *)
(* (* 0.923879532511 *) *)
(* (* 0.382683432365 *) *)
(* (* -0.382683432365 *) *)
(* (* -0.923879532511 *) *)
(* (* -0.923879532511 *) *)
(* (* -0.382683432365 *) *)

(* print_float(cos(0.392699081699)); *)
(* print_float(cos(1.1780972451)); *)
(* print_float(cos(1.96349540849)); *)
(* print_float(cos(2.74889357189)); *)
(* print_float(cos(3.53429173529)); *)
(* print_float(cos(4.31968989869)); *)
(* print_float(cos(5.10508806208)); *)
(* print_float(cos(5.89048622548)) *)
(*  *)
(* 0.923879532511 *)
(* 0.382683432365 *)
(* -0.382683432365 *)
(* -0.923879532511 *)
(* -0.923879532511 *)
(* -0.382683432365 *)
(* 0.382683432365 *)
(* 0.923879532511 *)
(*  *)
(* print_float(cos(0.392699081699 +. pit2)); *)
(* print_float(cos(1.1780972451 +. pit2)); *)
(* print_float(cos(1.96349540849 +. pit2)); *)
(* print_float(cos(2.74889357189 +. pit2)); *)
(* print_float(cos(3.53429173529 +. pit2)); *)
(* print_float(cos(4.31968989869 +. pit2)); *)
(* print_float(cos(5.10508806208+.pit2)); *)
(* print_float(cos(5.89048622548+.pit2)) *)
(*  *)
(* print_float(atan(-0.5)); *)
(* print_float(atan(-1.0)); *)
(* print_float(atan(-2.0)); *)
(* print_float(atan(-3.0)); *)
(* print_float(atan(0.5)); *)
(* print_float(atan(1.0)); *)
(* print_float(atan(2.0)); *)
(* print_float(atan(3.0)) *)
(*  *)
(*  *)
