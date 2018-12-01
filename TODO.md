- SPOCK LEVEL TESTS
- Sort workloads as they come in (or not)
- treat the tolerations/taints/anti-affinity node thing

- Return a 400 (error) when nodes given without all having unique ids, or
  without all workloads (even previously assigned ones) having unique ids
- Parameterize the whole library by numeric type, so that tests can use ints
  and real life can use floats

