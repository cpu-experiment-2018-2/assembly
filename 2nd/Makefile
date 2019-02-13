# OCB = ocamlbuild -use-ocamlfind -use-menhir -I src

PACKS = ppx_deriving.show
CC = gcc
CFLAGS = -g -O2 -Wall
SOURCES = src/stub.c src/syntax.ml src/lexer.mll src/parser.mly src/rmSucJump.ml src/encode.ml src/eliminate_dead_code.ml src/vliw.ml src/bit32.ml src/bit64.ml src/main.ml
RESULT=assemble

all: byte-code byte-code-library 

clean:: nobackup


include OCamlMakefile

