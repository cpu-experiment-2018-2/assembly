(* extend の後に *)
open Syntax

let rec f x =
  match x with
  | JUMP s1 :: Label s2 :: y when s1 = s2 -> Label s2 :: f y
  | x :: y -> x :: f y
  | _ -> x
