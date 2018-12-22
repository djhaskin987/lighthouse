.. _API Reference:

Tour of Features
================

This article constitutes a tour of features of Lighthouse, starting with
simplest use cases and moving on to more advanced features associated
with more complicated problems.

Lighthouse currently defines a single HTTP JSON RPC endpoint named
``/assign-workloads``.

Basic Scheduling
----------------

You can schedule workloads onto nodes like this::

    curl \
        -X POST \
        --data-binary '

    {
        "nodes": [
            {
                "id": "node-1",
                "resources": {
                    "cpu": 1.0,
                    "mem": 8.0
                    }
            },
            {
                "id": "secondnode",
                "resources": {
                    "cpu": 5.0,
                    "mem": 4.0
                }
            }
        ],
        "workloads": [
            {
            "id": "firstreq",
            "requirements": {
                "cpu": 5.0
                }
            }
        ],
        "strategy": "Prioritized"
    }' \
    <sphinx-endpoint>/assign-workloads

    # =>

    {
        "successful": true,
        "assignments": {
            "firstreq": "secondnode"
        }
    }

As you can see, ``assign-workloads`` takes a list of nodes and workloads
and attempts to assign workloads to nodes. If there is enough room,
it returns with ``successful`` as ``true`` and gives a list of assignments
using ``json``. If it fails, it will simply return a json blob with
``successful`` as ``false`` and an empty ``assignments`` dictionary::

    curl \
        -X POST \
        --data-binary '

    {
        "nodes": [
            {
                "id": "node-1",
                "resources": {
                    "cpu": 1.0,
                    "mem": 8.0
                    }
            },
            {
                "id": "secondnode",
                "resources": {
                    "cpu": 5.0,
                    "mem": 4.0
                }
            }
        ],
        "workloads": [
            {
            "id": "firstreq",
            "requirements": {
                "cpu": 6.0
                }
            }
        ],
        "strategy": "Prioritized"
    }' \
    <sphinx-endpoint>/assign-workloads

    # =>

    {
        "successful": false,
        "assignments": {}
    }

In the above example, workload placement failed because 

Failure to Schedule
-------------------

Placement Strategies
--------------------

Prioritized
+++++++++++

RoundRobin
++++++++++

BinPack
+++++++

Node Tagging
------------

Deficits and Tolerations
------------------------

This concept is similar to Kubernetes' `Taints and Tolerations`_ idea, but also
has nuances to it that make it more flexible.

Aversion Groups
---------------

.. _Taints and Tolerations: https://kubernetes.io/docs/concepts/configuration/taint-and-toleration/
