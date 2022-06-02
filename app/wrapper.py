#!/bin/python
# The ansible python module execution wrapper
import os, sys, logging, json, importlib

def loggy(msg):
    print(f" [python] {msg}", file=sys.stderr)

def call_module(mod, args):
    # Fork and connect stdin/stdout through pipe
    ((stdin_read, stdin_write), (stdout_read, stdout_write)) = (os.pipe(), os.pipe())
    pid = os.fork()
    if pid == 0:
        # The child to execute the ansible module main
        os.close(stdin_write)
        os.close(stdout_read)
        sys.stdin = os.fdopen(stdin_read, "r")
        sys.stdout = os.fdopen(stdout_write, "w")
        mod.main()
        exit(0)
    else:
        os.close(stdin_read)
        os.close(stdout_write)
        stdin = os.fdopen(stdin_write, "w")
        stdout = os.fdopen(stdout_read, "r")
        # Send the argument
        stdin.write(json.dumps(dict(ANSIBLE_MODULE_ARGS=args)))
        stdin.close()
        # Collect the output
        output = stdout.read()
        stdout.close()
        return json.loads(output)

def run_tasks(inputs):
    [task, attr] = inputs
    mod = importlib.import_module(f"ansible.modules.{task}")
    loggy(f"Running module {task} with {attr}")
    res = call_module(mod, attr)
    loggy(f"-> {res}")
    return res

loggy("Runner ready")
while True:
    cmd = sys.stdin.readline()
    if not cmd:
        break
    print(json.dumps(run_tasks(json.loads(cmd))), flush=True)
loggy("Runner completed")
