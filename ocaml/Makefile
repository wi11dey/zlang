zlang.byte: *.ml *.mli
	ocamlbuild $@

tests: zlang.byte tests.zl
	./$^ -q

clean:
	git clean -dfX

.PHONY: tests clean
