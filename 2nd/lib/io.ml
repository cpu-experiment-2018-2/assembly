
let rec mul10 x =
    let x2 = x + x in
    let x4 = x2 + x2 in
    let x8 = x4 + x4 in
    x8 + x2 in
let rec div10 x = 
    let rec div10binary x up low= 
        if up <= low + 1 then low else
        let mid =  div2 (up + low) in
        let mid10 = mul10 mid in
        if mid10 <= x then
            div10binary x up mid 
        else
            div10binary x mid low 
    in
        div10binary x x 0  
in 
let rec print_int_plus x flag = 
        let y = div10 x in
        let r = x - (mul10 y) in
        let nflag = if y = 0 then false else true in
        (if flag then  (print_int_plus y nflag;(print_char (48+r))) else ())
in
let rec print_int x = 
    if x = 0 then (print_char 48) else if x = 1 then (print_char 49) else
    let _ = 
    if x >= 0 then
        print_int_plus x true
    else 
        (print_char 45;
        print_int_plus (0-x) true
        )
    in 
    ()

in
let rec print_float_plus x = 
    if fless x 4000.0 then
        (
        print_int_plus (int_of_float x) true;print_char 46
        )
    else
        (
        let a = (floor x) in
        let b = x -. a in
        print_int_plus (int_of_float a) true ;print_char 46;
            (print_int_plus (int_of_float (b *. 1000.0)) true
         )
        )
in

let rec print_float x = 
    if fless 0.0 x  then
        print_float_plus x 
    else 
        (print_char 45;
        print_float_plus (0.0-.x) 
        )
in
()
