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

    curl \
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

Placement Enforcement
---------------------

At the time of placement of a workload onto a node, the requirements are
subtracted from the node's resources so as to keep track of what nodes still
have room left for more assignments. In particular, all attributes associated
with the *node* must register with a quantity at or above zero in order for the
assignment to succeed at *assignment time*.

This allows for some interesting possibilities for how to enforce where
workloads can be assigned in your cluster of nodes.

Node Tagging
++++++++++++

Sometimes it is desirable to mark a particular node as specifically dedicated
to a particular type of workload. When this is desired, it is simply a matter
of adding a resource to a node with zero as the quantity::

    ...
    "nodes": [
        {
            "id: "node1",
            "resources": {
               "dedicated": 0.0,
               ...
            }
        }
    ]

Then, simply place a similar attribute in the requirements dictionary
of the workloads that should be run on the dedicated nodes::

    ...
    "workloads": [
        {
            "id": "workload1",
            "requirements": {
                "dedicated": 0.0,
                ...
            }
        }
    ]

This works because all requirements listed for a workload must be present
on the node and none may be allowed to be below zero, but zero is okay.

Deficits and Tolerations
++++++++++++++++++++++++

This concept is similar to Kubernetes' `Taints and Tolerations`_ idea, but also
has nuances to it that make it more flexible.

The idea is to mark a particular set of nodes as unavailable for workloads
unless those workloads specifically opt into being run on those nodes.

We do this in Lighthouse using Defecits and Tolerations.

It is perfectly fine to list negative values for resources at call time on a
node; however, as has been previously explained, if there are any resources in
a node with negative quantity at *assignment time* of a workload, the workload
is not able to be attached.

Negative resources can be overcome by a resource in one of two ways.

First, for negative resource of *finite* this can be overcome by simply listing
a negative requirement. That way, when one is subtracted from the other, the
result will be zero::

    ...
    "nodes": [
        {
            "id: "node1",
            "resources": {
               "flies": -5.0,
               ...
            }
        }
    ],
    "workloads": [
        {
            "id": "workload1",
            "requirements": {
                "flies": -5.0,
                ...
            }
        }
    ]

This may be used to list "shortcomings" of a node that precludes it from having
workloads scheduled on it unless at least one workload has a sufficient
tolerance to the shortcoming.

Second, we list a node up front at call time with a resource that has infinite
negative value::

    ...
    "nodes": [
        {
            "id: "node1",
            "resources": {
               "spiders": -inf,
               ...
            }
        }
    ]

In this scenario, workloads will not be able to overcome the shortcoming no
matter how finitely resilient the workload is. However, we can list a
``toleration`` on the workload.

A ``toleration`` in a workload tells Lighthouse to ignore whatever value exists
for a resource in a node at assignment time of the workload. So, in order to
schedule a workload on the node listed above, we can simply add ``"spiders"``
to the toleration list for the workload::

    ...
    "workloads": [
        {
            "id": "workload1",
            "requirements": {
                ...
            },
            "tolerations": [
                "spiders",
                ...
            ]
        }
    ]

Aversion Groups
---------------

Aversion Groups correspond to anti-affinity groups in other scheduling schemes.

Put simply, any aversion group listed for a workload causes that workload
to "prefer" to be scheduled on a node without any other workloads listed
as "belonging" to the same aversion group, like this:::

    ...
    "nodes": [
        {
            "id: "node1",
            "resources": {
               ...
            }
        },
        {
            "id: "node2",
            "resources": {
               ...
            }
        }

    ],
    "workloads": [
        {
            "id": "workload1",
            "requirements": {
                ...
            },
            "aversion_groups": [
                "io-bound",
                ...
            ]
        },
        {
            "id": "workload2",
            "requirements": {
                ...
            },
            "aversion_groups": [
                "io-bound",
                ...
            ]
        }
    ]

In the above example, both ``workload1`` and ``workload2`` will try really hard
to be scheduled on different nodes, becuase they both list the ``io-bound``
aversion group in their aversion groups list.

.. _Taints and Tolerations: https://kubernetes.io/docs/concepts/configuration/taint-and-toleration/
