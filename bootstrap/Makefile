SWIPL ?= swipl
CLANG ?= clang
PYTHON ?= python3
LLVMFLAGS ?= -O2 -Wno-override-module

.DELETE_ON_ERROR:
.PHONY: all bootstrap test clean

all: build/zlang

build:
	mkdir -p $@

build/interpreter.ll: interpreter.zl emit.pl ../zlang.pl | build
	$(SWIPL) -q -l emit.pl -g emit_main -t halt -- interpreter.zl > $@.tmp
	mv $@.tmp $@

build/zlang: build/interpreter.ll
	$(CLANG) $(LLVMFLAGS) $< -o $@

build/stage2.ll: build/zlang interpreter.zl
	build/zlang --emit interpreter.zl > $@.tmp
	mv $@.tmp $@

build/zlang-stage2: build/stage2.ll
	$(CLANG) $(LLVMFLAGS) $< -o $@

build/stage3.ll: build/zlang-stage2 interpreter.zl
	build/zlang-stage2 --emit interpreter.zl > $@.tmp
	mv $@.tmp $@

bootstrap: build/stage3.ll
	cmp build/interpreter.ll build/stage2.ll
	cmp build/stage2.ll build/stage3.ll
	@echo "Bootstrap verified: all three LLVM modules are identical."

test: bootstrap
	$(PYTHON) tests/run.py build/zlang build/zlang-stage2

clean:
	rm -rf build
