SCM=guile

zlang: zlang-bootstrap zlang.zl
	./zlang-bootstrap zlang.zl > $@

zlang-bootstrap: zzlang.scm zlang.zl
	$(SCM) zzlang.scm zlang.zl > $@
	chmod u+x $@

clean:
	rm zlang zlang-bootstrap

test: zzlang.scm test/zzlang.scm
	cd test && $(SCM) zzlang.scm

.PHONY: clean test
