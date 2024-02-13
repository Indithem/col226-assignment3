
.PHONY: main
main: main.ml
	@echo "-----------------main start------------------------------"
	@cat test_cases1.ay | ocaml $<
	@echo "-----------------test_cases1 done------------------------"
	@cat test_cases2.ay | ocaml $<
	@echo "-----------------test_cases2 done------------------------"
	@cat test_cases3.ay | ocaml $<
	@echo "-----------------test_cases3 done------------------------"

.PHONY: test
test: test.ml
	@ocaml $<

%.ml: %.mll
	@ocamllex $<