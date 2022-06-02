#!/bin/python
# The ansible python module execution wrapper
import sys, json, os
from ansible.cli import CLI
from ansible.playbook.play import Play
from ansible.executor.task_queue_manager import TaskQueueManager
from ansible.plugins.callback import CallbackBase

def loggy(msg):
    print(f" {os.getpid()} [python] {msg}", file=sys.stderr)

# A callback to record task results
class CallbackModule(CallbackBase):
    CALLBACK_VERSION = 2.0
    CALLBACK_TYPE = 'notification'
    CALLBACK_NAME = 'log_plays'
    CALLBACK_NEEDS_WHITELIST = True

    def __init__(self):
        super(CallbackModule, self).__init__()
        self.results = []

    def runner_on_ok(self, _host, result):
        self.results.append(result)

    def runner_on_failed(self, _host, result, ignore_errors=False):
        self.results.append(result)

    def runner_on_skipped(self, _host, result):
        self.results.append(result)

# A fake cli to setup ansible global context
class Haxible(CLI):
    def init_parser(self):
        super().init_parser()

    def post_process_args(self, opts):
        opts.vault_ids = []
        opts.vault_password_files = []
        opts.ask_vault_pass = None
        opts.inventory = ""
        return super().post_process_args(opts)

    def run():
        ...
cli = Haxible(["haxible"])
cli.parse()
loader, inventory, variable_manager = cli._play_prereqs()

def run_task(inputs):
    [host, name, task, attr, env] = inputs
    loggy(f"{host}: Running task {name} ({task} with {attr}) env: {env}")
    try:
        cb = CallbackModule()
        tqm = TaskQueueManager(
            inventory=inventory, variable_manager=variable_manager,
            loader=loader, passwords=None, forks=5, stdout_callback=cb)
        play = Play().load(dict(
            name="Haxible Play", hosts=host, gather_facts='no', tasks=[{task: attr}], vars=env))
        tqm.run(play)
        if len(cb.results) != 1:
            loggy(f"The impossible has happen!: {cb.results}")
            exit(1)
        res = cb.results[0]
        tqm.cleanup()
    except Exception:
        raise
    loggy(f"-> {res}")
    return res

# run_task(["stat", dict(path="/etc/zuul")])
# run_task(["template", dict(dest="/tmp/conf", src="foo.j2")])
# run_task(["file", dict(path="/etc/zuul", state="directory")])

loggy("Runner ready")
while True:
    cmd = sys.stdin.readline()
    if not cmd:
        break
    print(json.dumps(run_task(json.loads(cmd))), flush=True)
loggy("Runner completed")
