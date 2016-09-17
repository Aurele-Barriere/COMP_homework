all:
	ocamlc -pp "camlp4o" expr.ml -o expr
clean :
	rm expr
