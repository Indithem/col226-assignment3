
.PHONY: main
main: main.ml
	@ocaml $<

.PHONY: test
test: test.ml
	@ocaml $<

%.ml: %.mll
	@ocamllex $<