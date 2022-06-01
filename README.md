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


## Overview

Haxible does the following:

- Parse playbook and annotate task dependencies, e.g. when a task attribute contains a registered variable name.
- Generate a Haxl computation.
- Evaluate the jinja templates.
- Execute the tasks through a persistent Python process using the JSON Lines protocol.

The implementation is presently missing:

- Dynamic vars such as host vars or `include_vars`
- Control flow such as `when` or `block`.
- Ansible python module usage: the action just returns fake uid.
- Remote execution: the python process runs locally.


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
      create_storage:
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
  ▶ Running create_storage with {'name': 'db'}
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
  --dry                    Don't run the playbook, just compile it
```

Run the tests with `cabal test`.

Build the command line with `cabal build`.
