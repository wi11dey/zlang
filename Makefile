SCM=guile

zlang: zlang-bootstrap zlang.zl
	./zlang-bootstrap zlang.zl > $@

zlang-bootstrap: zzlang.scm zlang.zl
	$(SCM) zzlang.scm zlang.zl > $@
	chmod u+x $@

clean:
	rm zlang zlang-bootstrap
