PHONY: run clean doc

IOCAML  = $(shell opam config var iocamljs-kernel:lib)
STATIC  = static/services/kernels/js/kernel.2048.js
PROFILE = profile/static/services/kernels/js/kernel.js
DOC     = doc/index.html
FILES   = static notebooks doc index.html basics.html adt.html modules.html

all: run
	@

doc: $(DOC)
	@

$(STATIC) $(PROFILE):
	cd 2048 && ./build
	jsoo_mktop -verbose \
	  -dont-export-unit gc \
	  -export-package react \
	  -export-package gg \
	  -export-package vg.htmlc \
	  -export-package js_of_ocaml \
	  -export-package lwt \
	  -export-package iocamljs-kernel \
	  -export-unit useri \
	  -export-unit useri_jsoo \
	  -export-unit utils \
	  -export-unit anim \
	  -export-unit g2048 \
	  -export-unit main \
	  -jsopt +weak.js -jsopt +toplevel.js \
	  -jsopt -I -jsopt ./2048/_build/useri \
	  -jsopt -I -jsopt ./2048/_build/src \
	  -jsopt -I -jsopt ./2048/_build/tests \
	  ./2048/_build/src/utils.cmo \
	  ./2048/_build/useri/useri_base.cmo \
	  ./2048/_build/useri/useri_backend_jsoo.cmo \
	  ./2048/_build/useri/useri_backend.cmo \
	  ./2048/_build/useri/useri.cmo \
	  ./2048/_build/useri/useri_jsoo.cmo \
	  ./2048/_build/src/g2048.cmo \
	  ./2048/_build/src/anim.cmo \
	  ./2048/_build/src/render.cmo \
	  ./2048/_build/src/main.cmo \
	  -o iocaml.byte
	mkdir -p $(shell dirname $(PROFILE))
	mkdir -p $(shell dirname $(STATIC))
	cat *.cmis.js $(IOCAML)/kernel.js iocaml.js > $(PROFILE)
	cp $(PROFILE) $(STATIC)

run: $(PROFILE) $(STATIC)
	iocaml -static profile notebooks

$(DOC):
	cd 2048 && ./build doc

clean:
	cd 2048 && ./build clean
	rm -f *.js *~ $(PROFILE) $(STATIC) iocaml.byte $(DOC)

update: $(PROFILE) $(STATIC) $(DOC)
	rsync -avz -e "ssh $(PORT)" $(FILES) $(DEST)

-include Makefile.private
