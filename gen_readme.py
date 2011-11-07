#!/usr/bin/python
# -*- coding: utf-8 -*-

import os,os.path,re,sys
from subprocess import Popen, PIPE

if len(sys.argv) != 3:
    sys.stderr.write("Usage: gen_readme.py INPUT OUTPUT")

if sys.argv[1] == "-":
    ifs = sys.stdin
else:
    ifs = open(sys.argv[1], 'r')

if sys.argv[2] == "-":
    ofs = sys.stdout
else:
    ofs = open(sys.argv[2], 'w')

while True:
    ln = ifs.readline()

    if len(ln) == 0: break

    ln = ln.strip()
    m = re.match('^<%(.*)%>$', ln)
    if m:
        cmdline = m.group(1)
        lines = Popen(['sh', '-c', cmdline], stdout=PIPE, stderr=PIPE).communicate()[0]

        for l in re.split(r'\r|\n|\r\n', lines):
            ofs.write("   %s\n" % l)
    else:
        ofs.write(ln + "\n")

ifs.close()
ofs.close()


