#!/usr/bin/env python3
"""Exercise persistent sessions, recovery, and real terminal prompt flushing."""

import os
from pathlib import Path
import pty
import select
import subprocess
import sys
import termios
import time

HERE = Path(__file__).resolve().parent
BINARIES = [str(Path(arg).resolve()) for arg in sys.argv[1:]]


def session(binary, args, source, stdout, stderr=b"", status=0):
    result = subprocess.run([binary, *args], input=source, capture_output=True, timeout=8)
    assert (result.returncode, result.stdout, result.stderr) == (status, stdout, stderr), result


def check_sessions(binary):
    for args in ([], ["--repl"]):
        session(binary, args, b"", b"")
        session(binary, args, b"\n \t\r\n", b"")
        session(binary, args, b"last-without-newline", b"last-without-newline\n")
        session(binary, args, b"(define 'x old)\nx\n(define 'x new) x\nx", b"old\nnew\nnew\n")
        session(binary, args, "(define 'réponse λ)\nréponse\n".encode(), "λ\n".encode())
        session(binary, args, b"(define\n `(id ,value)\n value)\n(id\n yes)\n", b"yes\n")
        session(binary, args, (HERE / "total-laziness.zl").read_bytes(), b"passed\n")
        # The validator checks the entire submission before executing it.
        session(binary, args, b"(define 'discarded yes) )\ndiscarded\n", b"discarded\n", b"syntax error: malformed input\n")
        session(binary, args, b"(define 'x kept)\n)\nnul\0byte\n' x\nx\n", b"kept\n", b"syntax error: malformed input\n" * 3)
        session(binary, args, b"(define 'x kept)\n(define x lost)\nx\n(define)\nx\n", b"kept\nkept\n", b"syntax error: definition requires quote or quasiquote\nsyntax error: expected (define 'name body) or (define `pattern body)\n")
        session(binary, args, b"(define 'x kept)\n(undefined)\n() x\n", b"kept\n", b"semantic error: no atom result\n" * 2)
        session(binary, args, b"(define 'x kept)\nx\n(define 'x", b"kept\n", b"syntax error: incomplete input at end of file\n", 1)
        session(binary, args, b"'", b"", b"syntax error: incomplete input at end of file\n", 1)
        # Parentheses and quote characters have the existing reader's meaning.
        session(binary, args, b"a'`b,c;d\n", b"a'`b,c;d\n")
        session(binary, args, b"x" * 70000 + b"\n", b"x" * 70000 + b"\n")
        # Repeated malformed input must not terminate the session.
        session(binary, args, b")\n" * 100 + b"still-running\n", b"still-running\n", b"syntax error: malformed input\n" * 100)


def check_terminal(binary):
    master, slave = pty.openpty()
    attributes = termios.tcgetattr(slave)
    attributes[3] &= ~termios.ECHO
    termios.tcsetattr(slave, termios.TCSANOW, attributes)
    process = subprocess.Popen([binary], stdin=slave, stdout=slave, stderr=slave)
    os.close(slave)
    pending = b""

    def expect(wanted):
        nonlocal pending
        deadline = time.monotonic() + 5
        while len(pending) < len(wanted):
            remaining = deadline - time.monotonic()
            assert remaining > 0, (wanted, pending)
            ready, _, _ = select.select([master], [], [], remaining)
            assert ready, (wanted, pending)
            pending += os.read(master, 4096).replace(b"\r\n", b"\n")
        assert pending.startswith(wanted), (wanted, pending)
        pending = pending[len(wanted):]

    try:
        expect(b"zlang> ")
        os.write(master, b"(define\n")
        expect(b"...> ")
        os.write(master, b" 'answer passed)\n")
        expect(b"zlang> ")
        os.write(master, b"answer\n")
        expect(b"passed\nzlang> ")
        os.write(master, b")\n")
        expect(b"syntax error: malformed input\nzlang> ")
        os.write(master, b"answer\n")
        expect(b"passed\nzlang> ")
        os.write(master, b"\x04")
        expect(b"\n")
        assert process.wait(timeout=5) == 0
        assert pending == b"", pending
    finally:
        if process.poll() is None:
            process.kill()
        process.wait()
        os.close(master)


if not BINARIES:
    sys.exit("usage: repl.py compiled-interpreter [another-compiled-interpreter ...]")
for binary in BINARIES:
    check_sessions(binary)
    check_terminal(binary)
print(f"Passed piped and terminal REPL checks for {len(BINARIES)} interpreter(s).")
