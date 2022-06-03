#!/bin/python
# The ansible python module execution wrapper
import sys, json, os
from ansible.cli import CLI
from ansible.playbook.play import Play
from ansible.executor.task_queue_manager import TaskQueueManager
from ansible.plugins.callback import CallbackBase

def loggy(msg):
    print(f" {os.getpid()} [python] {msg}", file=sys.stderr)

# PlaybookRunner is an ansible wrapper.
# The run function takes a playbook dictionary and it returns the task result.
class PlaybookRunner:
    def __init__(self, inventory):
        # A fake cli to setup ansible global context
        class Haxible(CLI):
            def init_parser(self):
                super().init_parser()
                self.parser.add_argument("--inventory")

            def post_process_args(self, opts):
                opts.vault_ids = []
                opts.vault_password_files = []
                opts.ask_vault_pass = None
                return super().post_process_args(opts)

            def run():
                ...
        self.cli = Haxible(["haxible", "--inventory", inventory])
        self.cli.parse()
        self.loader, self.inventory, self.variable_manager = self.cli._play_prereqs()

    def run(self, playbook):
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

        cb = CallbackModule()
        tqm = TaskQueueManager(
            inventory=self.inventory, variable_manager=self.variable_manager,
            loader=self.loader, passwords=None, forks=5, stdout_callback=cb)
        play = Play().load(playbook)
        run_result = tqm.run(play)
        if len(cb.results) != 1:
            loggy(f"The impossible has happen!: {cb.results}")
            exit(1)
        task_result = cb.results[0]
        tqm.cleanup()
        return [run_result, task_result]

# TODO: pass inventory path
runner = PlaybookRunner("test/inventory")

def run_task(inputs):
    [play, task, env] = inputs
    if env:
        play.setdefault("vars", {})
        play["vars"].update(env)
    loggy(f"{json.dumps(play)}: Running task {json.dumps(task)}")
    play["tasks"] = [task]
    play["gather_facts"] = "no"
    result = runner.run(play.copy())
    loggy(f"-> {result}")
    result[1]["__play"] = play
    return result

# run_task([dict(hosts="zuul_scheduler"), dict(stat=dict(path="/etc/zuul")), {}])

loggy("Runner ready")
while True:
    cmd = sys.stdin.readline()
    if not cmd:
        break
    print(json.dumps(run_task(json.loads(cmd))), flush=True)
loggy("Runner completed")
