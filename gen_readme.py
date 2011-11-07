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

    m = re.match('^<%(.*)%>$', ln.strip())
    if m:
        cmdline = m.group(1)
        lines = Popen(['sh', '-c', cmdline], stdout=PIPE, stderr=PIPE).communicate()[0]


        for l in re.split(r'\r|\n|\r\n', lines):
            l = re.sub(r'user1/\d\d[BMD]\d\d\d\d\d', '00M00000', l)
            ofs.write("     %s\n" % l)
    else:
        ofs.write(ln)

ifs.close()
ofs.close()


