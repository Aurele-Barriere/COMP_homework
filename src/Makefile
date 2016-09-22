all: expr_parser generate

expr_parser : expr_parser.ml
	ocamlc -pp "camlp4o" expr_parser.ml -o expr_parser

generate : generate.ml
	ocamlc generate.ml -o generate


clean :
	rm expr_parser
	rm generate
	rm *.cmi
	rm *.cmo
