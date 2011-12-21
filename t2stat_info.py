#!/usr/bin/python

import os,sys,re
from subprocess import Popen, PIPE

def get_output(cmd):
    return Popen(cmd, stdout=PIPE).communicate()[0]

def count_indent(s):
    m = re.match(r'^([ \t]*)[^ \t]', s)
    if m:
        return len(m.group(1))
    else:
        return 0

class QueueInfo:
    def __init__(self, raw_data = None):
        if raw_data is None:
            raw_data = get_output(['t2stat', '-Qf'])
            
        self.parse(raw_data)

    def parse(self, data):
        # Parse t2stat's output with a FSM
        self.queues = []
        cur_q = {}
        cur_key = None
        cur_val = None

        lines = re.split(r'\r\n|\r|\n', data)
        
        for ln in lines:
            if re.match(r'^\s*$', ln): continue

            elif re.match(r'^[^ \t]', ln):
                # beginning of self.queue
                if len(cur_q) > 0:
                    self.queues.append(cur_q)
                    cur_q = {}
                m = re.match(r'Queue: (\w+)', ln)
                if m:
                    cur_q['name'] = m.group(1)
                else:
                    raise RuntimeError("Can't parse line : '%s'" % ln)

            elif re.match(r'^ ', ln):
                if cur_key and cur_val:
                    cur_q[cur_key] = cur_val
                    cur_key = None
                    cur_val = None
                m = re.match(r' +([A-Za-z0-9_.:-]+) = ([A-Za-z0-9_.:, [\]=+-]+)', ln)
                cur_key = m.group(1)
                cur_val = m.group(2)

            elif re.match(r'^\t', ln):
                # Cont. of a property value from the previous line
                m = re.match(r'\s+([^ \t].*)$', ln)
                cur_val += m.group(1)
                
def main():
    qinfo = QueueInfo()

    for q in qinfo.queues:
        print q['name']
        print q['acl_groups']
    
main()
