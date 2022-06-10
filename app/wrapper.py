#!/bin/python
# This module contains two API.
#
# - TaskRunner: an interface to run individual individual task.
# - main: a REPL to receive command from Haxible.
import sys, json, os
from ansible.cli import CLI
from ansible.playbook.play import Play
from ansible.executor.task_queue_manager import TaskQueueManager
from ansible.plugins.callback import CallbackBase
from ansible.plugins.loader import module_loader
from ansible.template import Templar
from ansible.vars.hostvars import HostVars
from ansible.playbook.play_context import PlayContext
from ansible.executor.play_iterator import PlayIterator
from ansible.plugins.loader import strategy_loader


def loggy(msg):
    if os.environ.get("HAXIBLE_DEBUG"):
        print(f" {os.getpid()} [python] {msg}", file=sys.stderr)


class TaskRunner:
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

        # A callback to record task results per hosts.
        class CallbackModule(CallbackBase):
            CALLBACK_VERSION = 2.0
            CALLBACK_TYPE = 'notification'
            CALLBACK_NAME = 'log_plays'
            CALLBACK_NEEDS_WHITELIST = True

            def add_result(self, host, result, ignore_errors=False):
                # The play only has a single task.
                if host in self.results:
                    print(f"Multiple result for {host} {result}: {self.results}", file=sys.stderr)
                    exit(1)
                self.results[host] = result

            runner_on_ok = add_result
            runner_on_failed = add_result
        self.cb = CallbackModule()

        # The tqm handles the inventory and task execution
        self.tqm = TaskQueueManager(
            inventory=self.inventory, variable_manager=self.variable_manager,
            loader=self.loader, passwords=None, forks=5, stdout_callback=self.cb)
        # Pre load some of the tqm components
        self.tqm.load_callbacks()
        self.tqm._initialize_processes(5)

        self.templar = Templar(loader=self.loader)
        self.hostvars = HostVars(
            inventory=self.inventory,
            variable_manager=self.variable_manager,
            loader=self.loader,
        )
        self.strategy = strategy_loader.get("linear", self.tqm)

    def run(self, playbook, facts=None):
        loggy(f"Running {playbook}")
        for (host, host_facts) in (facts.items() if facts else []):
            self.variable_manager.set_host_facts(host, host_facts)
        # Host facts must be gathered manually using the ansible.builtin.gather_facts module
        playbook["gather_facts"] = False
        play = Play().load(playbook)
        # Simulate the tqm.run() function to perform our single task play
        all_vars = self.variable_manager.get_vars(play=play)
        self.templar._avaiable_variables = all_vars
        play_context = PlayContext(play, self.tqm.passwords)
        iterator = PlayIterator(
            inventory=self.inventory,
            play=play,
            play_context=play_context,
            variable_manager=self.variable_manager,
            all_vars=all_vars,
        )
        self.cb.results = {}
        run_result = self.strategy.run(iterator, play_context)

        if len(self.cb.results) == 0:
            # The run must produce a result.
            print("No result found!", file=sys.stderr)
            exit(1)
        elif len(self.cb.results) == 1:
            # If the task run on a single host, returns its value directly.
            [(host, value)] = list(self.cb.results.items())
            task_result = {**value, **{"__haxible_host": host}}
        else:
            # For multi host results, the output is a Map Host Result.
            # Later, the registered value will be substituted back
            # using this jinja expension: `{{ registered_result[ansible_host] }}`
            task_result = {**self.cb.results, **{"__haxible_multi_hosts": True}}

        # Returns the code and the result.
        result = [run_result, task_result]
        loggy(f"-> {json.dumps(result)}")
        return result

    async def async_run(self, playbook):
        return self.run(playbook)

def test():
    # Run test using: `python3 -c "import wrapper; wrapper.test()"`
    os.environ["HAXIBLE_DEBUG"] = "1"

    # Create the runner with an empty inventory
    runner = TaskRunner("")
    play = dict(hosts="localhost", tasks=[dict(command="echo Hello World")])
    print(json.dumps(runner.run(play)))

    async def main():
        play = dict(hosts="localhost", tasks=[dict(command="echo Hello World")])
        async_task = runner.async_run(play)
        print("Running...")
        print(await async_task)
    import asyncio
    asyncio.run(main())

def main():
    try:
        (inventory, playPath) = (sys.argv[1], sys.argv[2])
    except IndexError:
        print("usage: wrapper.py inventory playbook")
        exit(1)
    runner = TaskRunner(inventory)
    # TODO: check if ansible.cfg needs to be supplied.
    paths = set()
    def add_library_path(path):
        if path not in paths and path and os.path.isdir(path + "/library"):
            module_loader.add_directory(path + "/library")
            paths.add(path)
    os.chdir(os.path.dirname(playPath))
    add_library_path(os.path.dirname(playPath))

    def run_task(inputs):
        # The Haxl DataSource provides the play without tasks, the task to run
        # and extra environment vars for the play.
        [path, play, task, taskVars, facts] = inputs
        add_library_path(path)

        # Prepare the final play structure.
        if taskVars:
            play.setdefault("vars", {})
            play["vars"].update(taskVars)
        play["tasks"] = [task]
        # Call ansible-playbook
        result = runner.run(play.copy(), facts)
        # Record the full play structure in the task for debug purpose.
        result[1]["__haxible_play"] = play
        return result

    loggy("Runner ready")
    while True:
        cmd = sys.stdin.readline()
        if not cmd:
            break
        print(json.dumps(run_task(json.loads(cmd))), flush=True)
    loggy("Runner completed")
    runner.tqm.cleanup()
    runner.loader.cleanup_all_tmp_files()

if __name__ == "__main__":
    main()
