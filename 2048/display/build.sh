#!/bin/sh
set -e
ocamlbuild -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax,gg,vg.htmlc -syntax camlp4o game.byte
js_of_ocaml game.byte