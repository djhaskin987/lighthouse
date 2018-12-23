- Complete tour of features
- Add working examples to all sections in tour of features
- Add said examples into tests
- Profiling and optimization
- Partial list assignments
- ``Either`` support, so as to say what's wrong
  - and/or, partial placements (place what could be placed and report what
    couldn't)
- Return a 400 (error) when nodes given without all having unique ids, or
  without all workloads (even previously assigned ones) having unique ids
- Use the score assumption to increase the speed of the binpack algorithm,
  make it an error to give a negative rubric.
