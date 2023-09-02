.PHONY: all clean

all: schemata.info schemata.html index.html schemata.pdf ../README.md

schemata.info: schemata.texi
	makeinfo --no-validate schemata.texi

schemata.pdf: schemata.texi
	texi2any --force --no-validate --pdf schemata.texi -o schemata.pdf

schemata.html: schemata.texi
	texi2any --no-validate --html schemata.texi --css-ref texinfo-klare.css --css-ref lisp-coloring.css --no-split -o schemata.html
	sbcl --eval '(require :hyperspec)' --load colorize-lisp-examples.lisp schemata.html

index.html: schemata.texi
	texi2any --no-validate --html schemata.texi --css-ref texinfo-klare.css --css-ref lisp-coloring.css --no-split -o index.html
	sbcl --eval '(require :hyperspec)' --load colorize-lisp-examples.lisp index.html

schemata.texi:
	sbcl --eval '(ql:quickload :schemata)' --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "schemata.source.texi" "schemata.texi" :docsystem :texinfo :escape-docstrings nil :parse-docstrings nil)' --quit

schemata-internals.info: schemata-internals.weaved.texi
	makeinfo --no-validate schemata-internals.weaved.texi

schemata-internals.pdf: schemata-internals.weaved.texi
	texi2any --force --no-validate --pdf schemata-internals.weaved.texi -o schemata-internals.pdf

schemata-internals.html: schemata-internals.weaved.texi
	texi2any --no-validate --html schemata-internals.weaved.texi --css-ref texinfo-klare.css --no-split -o schemata-internals.html

schemata-internals.weaved.texi:
	sbcl --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "schemata-internals.texi" "schemata-internals.weaved.texi" :docsystem :texinfo)' --quit

clean:
	rm -f *.pdf
	rm -f *.html
	rm schemata.texi
