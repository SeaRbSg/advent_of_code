#lang brag

program     : instruction ("\n" instruction)
instruction : oper " " variable (" " variable)
oper        : "cpy" | "inc" | "dec" | "jnz"
integer     : digit+
variable    : (digit | address)
digit       : "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
address     : "a" | "b" | "c" | "d"
