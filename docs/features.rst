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
        ]
    }' \
    <sphinx-endpoint>/assign-workloads

    # =>
    #{
    #    "successful": true,
    #    "assignments": {
    #        "firstreq": "secondnode"
    #    }
    #}

As you can see, ``assign-workloads`` takes a list of nodes and workloads
and attempts to assign workloads to nodes. If there is enough room,
it returns with ``successful`` as ``true`` and gives a list of assignments
using ``json``.

Note that the requirements in a workload need not include all the types
of resources found in nodes. In the above example, each node has
``mem`` and ``cpu`` attributes, but only a ``cpu`` attribute is required
by the workload.

Failure to Schedule
-------------------

If it fails, it will simply return a json blob with
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
        ]
    }' \
    <sphinx-endpoint>/assign-workloads

    # =>
    #
    #{
    #    "successful": false,
    #    "assignments": {}
    #}

In the above example, workload placement failed because the workload needed
too much CPU. It needed ``6.0`` units of ``cpu``, but any single available
node given only had ``5.0``.

Placement of workloads onto nodes is not guaranteed. That is, simply because
room exists for all workloads, this does not mean that Lighthouse will be able
to figure this out. You can help Lighthouse get better at packing nodes tightly
using the `BinPack`_ strategy, and you can also increase the capacity of the
nodes.

Placement Strategies
--------------------

You can tell Lighthouse to use one of several placement strategies when
placingj workloads onto nodes. ``Prioritized`` is the default, because it is
the simplest, but it is not the best. ``BinPack`` is, in general, recommended.

The following example will be referred to when discussing each of the
placement strategies below::
    curl \k
        -X POST \
        --data-binary '
    {
      "nodes": [
        {
          "id": "node-1",
          "resources": {
            "cpu": 2,
            "mem": 8,
            "disk": 60
          }
        },
        {
          "id": "node-2",
          "resources": {
            "cpu": 6,
            "mem": 6,
            "disk": 20
          }
        },
        {
          "id": "node-3",
          "resources": {
            "cpu": 4,
            "mem": 2,
            "disk": 40
          }
        }
      ],
      "workloads": [
        {
          "id": "req-1",
          "requirements": {
            "cpu": 1,
            "mem": 2,
            "disk": 10
          }
        },
        {
          "id": "req-2",
          "requirements": {
            "cpu": 3,
            "mem": 2,
            "disk": 5
          }
        },
        {
          "id": "req-3",
          "requirements": {
            "cpu": 2,
            "mem": 4,
            "disk": 50
          }
        }
      ],
      "strategy": "<strategy>"
    }
    ' <lighthouse-endpoint>/assign-workloads

    # =>
    #{"successful":true,"assignments":{"req-1":"node-2","req-3":"node-1","req-2":"node-3"}}

Prioritized
+++++++++++

With a strategy of ``Prioritized``, Lighthouse will attempt to assign workloads
to nodes in the order they appear in the given list of nodes, and in the order
the workloads appear.

This is the result if the above were run with ``<strategy>`` were run with
``Prioritized``::

    {"successful":true,"assignments":{"req-1":"node-1","req-3":"node-1","req-2":"node-1"}}

In this example, all nodes are assigned to ``node-1`` because they can all
fit on ``node-1`` and it appears first in the list of nodes given.

RoundRobin
++++++++++

With a strategy of ``RoundRobin``, assignment of workloads is done in the order
given in the list, but placement attempts for each successive load starts on
the node just after the successful placement of the previous load -- in a
"round robin" fashion.

This is the result if the above were run with ``<strategy>`` as
``RoundRobin``::

    {"successful":true,"assignments":{"req-1":"node-1","req-3":"node-3","req-2":"node-2"}}

.. _BinPack:

BinPack
+++++++

This strategy requires additional information in the JSON blob that is given
to ``/assign-workloads``. A ``rubric`` must be specified. In discussing the
example above, we will assume in our discussion that the following was also
sent to the RPC endpoint::

    "strategy": "BinPack",
    "rubric": {
            "cpu": 1,
            "mem": 0.5,
            "disk": 0.025
          }
    ...

BinPack attempts to pack in as many requirements into as few nodes as possible.
In order to do so, the caller must specify a ``rubric``. This specifies that
certain attributes need to be present in all nodes as resources and all
workloads as requirements, and gives quantities that will be used to score
each workload and node by multiplying each quantity for a given node or
workload and summing the results. This score is computed for each node and
workload and semantically corresponds to the node or load's "size". If any node
or workload doesn't have all the attributes in the rubric, the call to
``/assign-workloads`` will not be successful. In future versions of
``/assign-workloads``, specifying negative values in the rubric will not be
allowed and in the current version if this happens the result is undefined.

If ``BinPack`` was used in the above example, the result would look like this::

{"successful":true,"assignments":{"req-1":"node-2","req-3":"node-1","req-2":"node-3"}}

In this example, all workloads were assigned to ``node-3``, since ``node-3``
had the least room in it going into scheduling, since it had the least disk
space.

Node Tagging
------------

Deficits and Tolerations
------------------------

This concept is similar to Kubernetes' `Taints and Tolerations`_ idea, but also
has nuances to it that make it more flexible.

Aversion Groups
---------------

.. _Taints and Tolerations: https://kubernetes.io/docs/concepts/configuration/taint-and-toleration/
