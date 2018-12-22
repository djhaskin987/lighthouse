# Lighthouse

It guides workloads to safe harbors -- just like a lighthouse!

Lighthouse is a generic workload scheduler.

# Some sort of docs

Lighthouse currently exposes one endpoint, "assign-workloads", which allows you
to schedule workloads to nodes. You can have lighthouse tell you where to put
your workloads by posting what workloads you need scheduled and what nodes you
have upon which to schedule them:

```bash

curl -X POST -d '{
    "nodes": [{"id": "node-1", "resources": {"cpu": 1.0, "mem": 8.0}},
              {"id": "secondnode", "resources": {"cpu": 5.0, "mem": 4.0}}],
    "workloads": [{"id": "firstreq", "requirements": {"cpu": 5.0}}],
    "strategy": "Prioritized"
}' localhost:8087/assign-workloads
# => { "assignments": [{"firstreq", "firstnode"}]}

```

Multiple workloads and nodes can be given. Lighthouse keeps track of which
nodes were assigned which to which nodes internally and makes sure that all
resources get their requirements met.

##
It keeps track of 
Lighthouse has the notion of "workloads" and "nodes". Nodes are collections of
resources to which workloads are assigned or scheduled. Workloads are
collections of requirements:

```json

{ "nodes": "
