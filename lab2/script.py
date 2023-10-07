#!/usr/bin/env python3
import subprocess
import sys
from sys import platform

if len(sys.argv) == 8:
    alphabet_len = int(sys.argv[1])
    regex_len = int(sys.argv[2])
    star_len = int(sys.argv[3])
    num_regex = int(sys.argv[4])
    word_len = int(sys.argv[5])
    num_tests = int(sys.argv[6])
    use_additional_alphabete = sys.argv[7]
    subprocess.call(f"scala main.jar -r {alphabet_len} {regex_len} {star_len} {num_regex}", shell=True)
    subprocess.call("rlmake ./main.ref", shell=True)
    if platform == 'linux' or platform == 'linux2':
        subprocess.call("./main regex.txt fsm.txt", shell=True)
    else:
        subprocess.call("./main.exe regex.txt fsm.txt", shell=True)
    subprocess.call(f"scala main.jar -w {word_len} {num_tests} {use_additional_alphabete}", shell=True)
else:
    sys.exit(1)