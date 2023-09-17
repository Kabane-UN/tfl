#!/usr/bin/env python3
import subprocess
import sys

if len(sys.argv) == 2 and sys.argv[1] == '-scala':
    subprocess.call("scala main.scala", shell=True)
elif len(sys.argv) == 2 and sys.argv[1] == '-jar':
    subprocess.call("scala main.jar", shell=True)
else:
    print("неверные аргументы")
    sys.exit(-1)
subprocess.call("z3 -smt2 input.smt2", shell=True)