# Haxible - Ansible interpreter powered by Haxl

This project is a proof concept Ansible interpreter that leverages
the [Haxl](https://github.com/facebook/Haxl) library to evaluate
the playbook tasks concurrently.


## Why Haxl?

Pros:

- Haxl is a big hammer for solving problems involving I/O and concurrency: it flips the default from sequential to concurrent.
- Haxl comes with testing and debugging facilities.

Cons:

- The implementation is not written in Python.

> Learn more about Haxl in this short video: https://www.youtube.com/watch?v=sT6VJkkhy0o

Why Haskell?

- Suitable for large scale system.
- Statically typed with type inference.
- Fearless concurrency, without async/await, but with managed effects (IO and STM).


## Overview

Haxible does the following:

- Parse the playbook into a syntax tree.
- Annotate the tasks dependencies.
- Generate a Haxl program.
- Compile the generated code with the Glasgow Haskell Compiler.
- Perform the tasks through the `ansible-playbook` command.

The implementation is presently missing:

- Dynamic vars such as `add_hosts` or `include_vars`.
- Control flow such as `block` or `include_tasks`.


## Demo

Given this playbook:

```yaml
- hosts: localhost
  tasks:
    - name: Create network
      create_network:
        name: "private"
      register: network

    - name: Create instances
      create_instance:
        network: "{{ network.uid }}"
        name: "{{ item }}"
      loop:
        - backend
        - frontend
        - monitoring

    - name: Create storage
      create_volume:
        name: "db"
      register: storage

    - name: Create database
      create_instance:
        network: "{{ network.uid }}"
        name: "database"
        volume: "{{ storage.uid }}"

    - name: Create object
      create_object:
        name: "standalone-object"

    - name: Create network object
      create_object:
        name: "network-{{ network.uid }}"

    - name: Start local service
      include_role:
        name: "container-service"
```

Haxible runs three batches:

```
[python] Runner ready
[+] Batching 2 tasks
  ▶ Running debug with {'msg': 'Starting local service'}
  ▶ Running create_network with {'name': 'private'}
[+] Batching 6 tasks
  ▶ Running create_object with {'name': 'network-create_network_private_uuid'}
  ▶ Running create_object with {'name': standalone-object'}
  ▶ Running create_volume with {'name': 'db'}
  ▶ Running create_instance {'name': 'monitoring', 'network': 'create_network_private_uuid'}
  ▶ Running create_instance {'name': 'frontend', 'network': 'create_network_private_uuid'}
  ▶ Running create_instance {'name': 'backend', 'network': 'create_network_private_uuid'}
[+] Batching 1 task
  ▶ Running create_instance {'name': 'database', 'network': 'create_network_private_uuid', 'volume': 'create_storage_db_uuid'}
[python] Runner completed
```

Thanks to the Haxl library, the order of the operations is automatically arranged to maximize concurrency.


## Usage

Install the toolchain using [ghcup](https://www.haskell.org/ghcup/).

```ShellSession
$ cabal run haxible -- --help
Haxible - Ansible interpreter powered by Haxl

Usage: haxible --playbook FILE [--dry]

Available options:
  --help                   Show this help text
  --playbook FILE          YAML file to interpret
  --inventory FILE         Inventory path
  --dry                    Don't run the playbook, just compile it
```

Run the tests with `cabal test`.

Build the command line with `cabal build`.
