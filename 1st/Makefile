# OCB = ocamlbuild -use-ocamlfind -use-menhir -I src

PACKS = ppx_deriving.show
CC = gcc
CFLAGS = -g -O2 -Wall
SOURCES = src/stub.c src/syntax.ml src/lexer.mll src/parser.mly src/encode.ml src/main.ml
RESULT=assemble

all: byte-code byte-code-library 

clean:: nobackup


include OCamlMakefile

