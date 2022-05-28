#!/bin/python
# The ansible python module execution wrapper
import sys, logging, json

def loggy(msg):
    print(f" [python] {msg}", file=sys.stderr)

def run_task(inputs):
    [action, attr] = inputs
    loggy(f"Running action {action} with {attr}")
    fake_uid = action + "_" + attr.get("name", "") + "_uuid"
    return dict(status="success", uid=fake_uid)

loggy("Runner ready")
while True:
    cmd = sys.stdin.readline()
    if not cmd:
        break
    print(json.dumps(run_task(json.loads(cmd))), flush=True)
loggy("Runner completed")
