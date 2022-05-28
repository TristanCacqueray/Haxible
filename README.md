# Haxible - Ansible interpreter powered by Haxl

This project is a proof concept Ansible interpreter that leverages
the [Haxl](https://github.com/facebook/Haxl) library to evaluate
the playbook tasks concurrently.

## Overview

Haxible does the following:

- Parse playbook and annotate task dependencies, e.g. when a task attribute contains a registered variable name.
- Generate a Haxl computation.
- Evaluate the jinja templates.
- Execute the tasks through a persistent Python process using the JSON Lines protocol.

The implementation is presently missing:

- Dynamic attributes and tasks such as `include_vars` or `include_role`.
- Control flow such as `when` or `block`.
- Ansible python module usage: the action just returns fake uid.
- Remote execution: the python process runs locally.


## Demo

Given these tasks:

```yaml
- name: Create network
  create_network:
    name: "private"
  register: network

- name: Create storage
  create_storage:
    name: "db"
  register: storage

- name: Create instances
  create_instance:
    network: "{{ network.uid }}"
    name: "{{ item }}"
  loop:
    - backend
    - frontend
    - monitoring

- name: Create database
  create_instance:
    network: "{{ network.uid }}"
    name: "database"
    volume: "{{ storage.uid }}"
```

Haxible runs two batches:

```
[python] Runner ready
[+] Batching 2 tasks
  ▶ Running create_storage with {'name': 'db'}
  ▶ Running create_network with {'name': 'private'}
[+] Batching 4 tasks
  ▶ Running create_database {'name': 'database', 'network': 'create_network_private_uuid', 'volume': 'create_storage_db_uuid'}
  ▶ Running create_instances {'name': 'monitoring', 'network': 'create_network_private_uuid'}
  ▶ Running create_instances {'name': 'frontend', 'network': 'create_network_private_uuid'}
  ▶ Running create_instances {'name': 'backend', 'network': 'create_network_private_uuid'}
[python] Runner completed
```

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
