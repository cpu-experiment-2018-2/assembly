{
open Parser
}

let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+
    { token lexbuf }
| "\n" {
    Lexing.new_line lexbuf;  
    token lexbuf
    }
| "(*"
    { comment lexbuf; 
      token lexbuf }
| "%sp"
{
    PERCENTINT(1)

}
| "%fp"
{
    PERCENTINT(2)
}
| "%lr"{
    PERCENTINT(31)
}
| "%r"digit+{
    let t =  (Lexing.lexeme lexbuf) in
    let t = String.sub t 2 ((String.length t)-2) in
    let s = int_of_string t in
    if s >= 32 then
        failwith ("register number should be <= 31, but found" ^ t) else 
    PERCENTINT(s)
    }
| "-"digit+{
        INT((int_of_string (Lexing.lexeme lexbuf))) 
    }
| digit+ 
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| ['-']? digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| ','
    { COMMA }
| eof
    { EOF }
| "addi" {ADDI }
| "add"  {ADD}
| "subi"  {SUBI}
| "sub"  {SUB}
| "muli"  {MULI}
| "mul"  {MUL}
| "divi"  {DIVI}
| "div"  {DIV}
| "fadd"  {FADD}
| "fsub"  {FSUB}
| "fmul"  {FMUL}
| "fdiv"  {FDIV}
| "srawi" { SRAWI}
| "slawi" { SLAWI}
| "mr"  {MR}
| "inll"  {INLL}
| "inlh"  {INLH}
| "inul"  {INUL}
| "inuh"  {INUH}
| "outll"  {OUTLL}
| "outlh"  {OUTLH}
| "outul"  {OUTUL}
| "outuh"  {OUTUH}
| "lis"  {LIS}
| "fli"  {FLI}
| "li"  {LI}
| "store"  {STORE}
| "load"  {LOAD}
| "cmpdi"  {CMPDI}
| "cmpd"  {CMPD}
| "beq"  {BEQ}
| "blrr"  {BLRR}
| "blr"  {BLR}
| "mr"  {MR}
| "inc"  {INC}
| "lil"  {LIL}
| "dec"  {DEC}
| "bl"  {BL}
| "end"  {END}
| "ble"  {BLE} | "jump"  {JUMP}
| ":"  {COLON}
| (upper|lower|'.'|"@@") (digit|lower|upper|'_'|'.')* 
    { IDENT(Lexing.lexeme lexbuf) }
| _ 
    { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| "\n" {
    Lexing.new_line lexbuf;
    comment lexbuf
}
| _
    { comment lexbuf }
