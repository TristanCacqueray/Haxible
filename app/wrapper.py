#!/bin/python
# The ansible python module execution wrapper.
# This code provides a performance boost by keeping the ansible
# modules code in memory. Though using the python library is not
# a requirements for Haxible, and the execution could be done
# using the ansible-playbook command line with a custom callback
# to record the task results.
import sys, json, os
from ansible.cli import CLI
from ansible.playbook.play import Play
from ansible.executor.task_queue_manager import TaskQueueManager
from ansible.plugins.callback import CallbackBase
from ansible.plugins.loader import module_loader

def loggy(msg):
    if os.environ.get("HAXIBLE_DEBUG"):
        print(f" {os.getpid()} [python] {msg}", file=sys.stderr)

# PlaybookRunner is the ansible abstraction.
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
        # A callback to record task results per hosts.
        class CallbackModule(CallbackBase):
            CALLBACK_VERSION = 2.0
            CALLBACK_TYPE = 'notification'
            CALLBACK_NAME = 'log_plays'
            CALLBACK_NEEDS_WHITELIST = True

            def __init__(self):
                super(CallbackModule, self).__init__()
                self.results = dict()

            def add_result(self, host, result, ignore_errors=False):
                # The play only has a single task.
                if host in self.results:
                    print(f"Multiple result for {host} {result}: {self.results}", file=sys.stderr)
                    exit(1)
                self.results[host] = result

            runner_on_ok = add_result
            runner_on_failed = add_result

        cb = CallbackModule()
        tqm = TaskQueueManager(
            inventory=self.inventory, variable_manager=self.variable_manager,
            loader=self.loader, passwords=None, forks=5, stdout_callback=cb)
        play = Play().load(playbook)
        run_result = tqm.run(play)
        if len(cb.results) == 0:
            print("No result found!", file=sys.stderr)
            exit(1)
        elif len(cb.results) == 1:
            task_result = list(cb.results.values())[0]
        else:
            # For multi host results, the output is a Map Host Result with a special key to indicate
            # the result is for multiple host. Later, the registered value will be substituted back
            # by the `Haxible.Eval.runTask` function.
            task_result = {**cb.results, **{"__haxible_multi_hosts": True}}
        tqm.cleanup()
        self.loader.cleanup_all_tmp_files()
        # Returns the code (0 for success, ...) and the result.
        return [run_result, task_result]

try:
    (inventory, playPath) = (sys.argv[1], sys.argv[2])
except IndexError:
    print("usage: wrapper.py inventory")
    exit(1)
runner = PlaybookRunner(inventory)
# TODO: check if ansible.cfg needs to be supplied.
paths = set()
def add_library_path(path):
    if path not in paths and path and os.path.isdir(path + "/library"):
        module_loader.add_directory(path + "/library")
        paths.add(path)
add_library_path(os.path.dirname(playPath))

def run_task(inputs):
    # The Haxl DataSource provides the play without tasks, the task to run
    # and extra environment vars for the play.
    [path, play, task, taskVars] = inputs
    add_library_path(path)

    # Prepare the final play structure.
    if taskVars:
        play.setdefault("vars", {})
        play["vars"].update(taskVars)
    loggy(f"{json.dumps(play)}: Running task {json.dumps(task)}")
    play["tasks"] = [task]
    play["gather_facts"] = "no"
    # Call ansible-playbook
    result = runner.run(play.copy())
    loggy(f"-> {result}")
    # Record the full play structure in the task for debug purpose.
    result[1]["__haxible_play"] = play
    return result

# run_task([dict(hosts="all"), dict(command="echo {{ ansible_host }}"), []])

loggy("Runner ready")
while True:
    cmd = sys.stdin.readline()
    if not cmd:
        break
    print(json.dumps(run_task(json.loads(cmd))), flush=True)
loggy("Runner completed")
