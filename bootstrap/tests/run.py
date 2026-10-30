#!/usr/bin/env python3
"""Differential tests against zlang.pl, plus standalone interpreter checks."""

import concurrent.futures
import os
from pathlib import Path
import random
import subprocess
import sys
import tempfile

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
BINARIES = [str(Path(arg).resolve()) for arg in sys.argv[1:]]
SWIPL = os.environ.get("SWIPL", "swipl")
CASES = []


def case(name, source, expected):
    CASES.append((name, source, expected))


case("atom", "answer", "answer")
case("numeric atoms", "12345", "12345")
case("UTF-8 atoms", "(define 'réponse λ) réponse", "λ")
case("ASCII whitespace", "\t\v\f(define 'x yes)\r\n x\t", "yes")
case("token punctuation", "a'`b,c;d", "a'`b,c;d")
case("newest constant", "(define 'x old) (define 'x new) x", "new")
case("alias chain", "(define 'x y) (define 'y yes) x", "yes")
case("quoted list definition", "(define '(f x) yes) (f x)", "yes")
case("quoted empty list", "(define '() yes) ()", "yes")
case("constant before pattern", "(define `(f ,x) no) (define '(f x) yes) (f x)", "yes")
case("oldest pattern", "(define `(f ,x) first) (define `(f ,x) second) (f x)", "first")
case("capture", "(define `(identity ,x) x) (identity yes)", "yes")
case("wildcard is lazy", "(define `(ignore _) yes) (ignore (undefined))", "yes")
case("unused capture is lazy", "(define `(ignore ,x) yes) (ignore (undefined))", "yes")
case("duplicate capture takes first", "(define `(f ,x ,x) x) (f first second)", "first")
case("substitution is simultaneous", "(define `(f ,x ,y) x) (f y yes)", "y")
case("substitution traverses quotation", "(define '(quote yes) passed) (define `(f ,x) 'x) (f yes)", "passed")
case("atom cut", "(define `,x wrong) plain", "plain")
case("whole expression capture", "(define `,x yes) (anything)", "yes")
case("literal nested list", "(define `(f (box yes)) passed) (f (box yes))", "passed")
case("empty list in a pattern", "(define `(f ()) passed) (f ())", "passed")
case("force a constant into a structure", "(define 'x (box yes)) (define `(f (box ,x)) x) (f x)", "yes")
case("force a function position", "(define 'g f) (define `(f ,x) x) (g yes)", "yes")
case("computed function position", "(define `(identity ,x) x) (define `(operator) identity) ((operator) yes)", "yes")
case("higher order function", "(define `(apply ,f ,x) (f x)) (define `(identity ,x) x) (apply identity yes)", "yes")
case("structured function position", "(define `((const ,x) _) x) ((const yes) (undefined))", "yes")
case("constant backtracking", "(define 'x yes) (define 'x (undefined)) x", "yes")
case("forcing backtracks over constants", "(define '(choice) right) (define '(choice) wrong) (define `(accept right) yes) (accept (choice))", "yes")
case("overload result backtracking", "(define `(f ,x) (undefined)) (define `(f ,x) yes) (f ignored)", "yes")
case("forcing backtracks over patterns", "(define `(choice) wrong) (define `(choice) right) (define `(accept right) yes) (accept (choice))", "yes")
case("different arities", "(define `(f) zero) (define `(f ,x) x) (f yes)", "yes")
case("different arities reversed", "(define `(f ,x) x) (define `(f) zero) (f)", "zero")
case("computed false condition", "(define `(if #t ,yes _) yes) (define `(if #f _ ,no) no) (define `(condition) #f) (if (condition) wrong passed)", "passed")
case("structural cut", "(define '(box a) (box b)) (define `(use (box b)) wrong) (define `(use _) yes) (use (box a))", "yes")
case("infinite stream", "(define 'ones (cons yes ones)) (define `(head (cons ,x _)) x) (head ones)", "yes")
case("lazy language examples", (HERE / "total-laziness.zl").read_text(), "passed")
case("large atom", "x" * 70000, "x" * 70000)

BOOLS = """
(define `(if #t ,yes _) yes)
(define `(if #f _ ,no) no)
(define `(not #t) #f)
(define `(not #f) #t)
(define `(and ,left ,right) (if left right #f))
(define `(or ,left ,right) (if left #t right))
(define `(loop) (loop))
"""
rng = random.Random(20261029)


def boolean(depth):
    if depth == 0:
        value = rng.choice([True, False])
        return ("#t" if value else "#f"), value
    kind = rng.choice(["if", "not", "and", "or", "lazy"])
    left, a = boolean(depth - 1)
    if kind == "not":
        return f"(not {left})", not a
    right, b = boolean(depth - 1)
    if kind == "and":
        return f"(and {left} {right})", a and b
    if kind == "or":
        return f"(or {left} {right})", a or b
    if kind == "lazy":
        return (f"(if {left} {right} (loop))" if a else f"(if {left} (loop) {right})"), b
    other, c = boolean(depth - 1)
    return f"(if {left} {right} {other})", b if a else c


for i in range(100):
    expression, expected = boolean(rng.randrange(1, 5))
    case(f"generated boolean {i}", BOOLS + expression, "#t" if expected else "#f")

NATURALS = """
(define `(same zero zero) #t)
(define `(same zero (succ _)) #f)
(define `(same (succ _) zero) #f)
(define `(same (succ ,a) (succ ,b)) (same a b))
"""


def natural(n):
    return "(succ " * n + "zero" + ")" * n


for i in range(30):
    a, b = rng.randrange(15), rng.randrange(15)
    case(f"recursive two argument dispatch {i}", NATURALS + f"(same {natural(a)} {natural(b)})", "#t" if a == b else "#f")


def run(command, expected, label):
    try:
        result = subprocess.run(command, capture_output=True, timeout=8)
    except subprocess.TimeoutExpired as error:
        raise AssertionError(f"{label}: {command[0]} timed out") from error
    if result.returncode or result.stdout != expected:
        raise AssertionError(f"{label}: {command[0]} exited {result.returncode}; stdout={result.stdout[:200]!r}; stderr={result.stderr[:400]!r}")


def check(item):
    name, source, expected = item
    with tempfile.TemporaryDirectory(prefix="zlang-test-") as directory:
        path = Path(directory) / "test.zl"
        path.write_text(source, encoding="utf-8")
        wanted = (expected + "\n").encode()
        run([SWIPL, "-q", "-l", str(HERE / "oracle.pl"), "-g", "oracle_main", "-t", "halt", "--", str(path)], wanted, name)
        for binary in BINARIES:
            run([binary, str(path)], wanted, name)


def check_errors_and_output():
    bad = ["", "(", ")", "'", "' x", "(define x yes) yes", "ordinary yes", "(undefined)", "()", "(define `(f ,()) yes) (f x)", "nul\0byte"]
    with tempfile.TemporaryDirectory(prefix="zlang-errors-") as directory:
        path = Path(directory) / "test.zl"
        for source in bad:
            path.write_text(source)
            for binary in BINARIES:
                result = subprocess.run([binary, str(path)], capture_output=True, timeout=8)
                assert result.returncode > 0 and b"error" in result.stderr, (source, result)
        for binary in BINARIES:
            for args in ([], [str(path) + ".missing"], ["--bad", str(path)]):
                result = subprocess.run([binary, *args], capture_output=True, timeout=8)
                assert result.returncode > 0 and result.stderr, (args, result)
        # The byte observer must be generic: this program emits no LLVM at all.
        path.write_text("(define `(if #t ,yes _) yes) (if #t (out_cat (out_text λ) (out_cat (out_char 255) (out_cat (out_char 0) (out_cat (out_char 10) (out_empty))))) (undefined))")
        wanted = "λ".encode() + b"\xff\0\n"
        run([SWIPL, "-q", "-l", str(HERE.parent / "emit.pl"), "-g", "emit_main", "-t", "halt", "--", str(path)], wanted, "Prolog output observer")
        for binary in BINARIES:
            run([binary, "--emit", str(path)], wanted, "compiled output observer")
        for source in ["(out_text ())", "(out_char -1)", "(out_char 256)", "(out_char nope)", "(out_char 999999999999999999999)", "(unknown)"]:
            path.write_text(source)
            for binary in BINARIES:
                result = subprocess.run([binary, "--emit", str(path)], capture_output=True, timeout=8)
                assert result.returncode > 0 and b"output error" in result.stderr, (source, result)


if not BINARIES:
    sys.exit("usage: run.py compiled-interpreter [another-compiled-interpreter ...]")
with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
    list(pool.map(check, CASES))
check_errors_and_output()
print(f"Passed {len(CASES)} differential cases against zlang.pl for {len(BINARIES)} interpreter(s), plus CLI/error/output checks.")
