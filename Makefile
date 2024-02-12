
.PHONY: main
main: main.ml
	@cat test_cases.ay | ocaml $<

.PHONY: test
test: test.ml
	@ocaml $<

%.ml: %.mll
	@ocamllex $<