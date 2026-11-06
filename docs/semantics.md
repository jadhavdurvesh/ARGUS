# ARGUS semantics

## Transition systems

A model is a finite set of integer-identified states. Each state carries a set of atomic propositions and a set of successor state IDs. A model also carries a set of initial state IDs.

Construction validates that state identifiers are unique, every successor exists, and every initial state exists.

## CTL evaluation

Each formula evaluates to the set of states satisfying it.

- Boolean operators use set complement, intersection and union relative to the model universe.
- EX uses existential predecessor: at least one successor is in the operand set.
- AX uses universal predecessor: every successor is in the operand set.
- EF and AF are least fixed points.
- EG and AG are greatest fixed points.
- EU and AU are least fixed points combining the left operand with the right-hand target.

The algorithms terminate because the state universe is finite and each fixed-point iteration is monotone over a finite powerset lattice.

## Deadlocks

For a state with no successors, AX is true and EX is false under the current predecessor definitions. This is vacuous universal semantics. Some model-checking conventions require total transition relations; ARGUS currently leaves that policy explicit rather than silently adding self-loops.

## Verdict

The evaluator returns the full satisfying-state set. The CLI verdict is true when every declared initial state belongs to that set. An empty initial set therefore satisfies the subset check; production use should declare at least one initial state. A future strict model mode will reject empty initial sets.

## Complexity

The current fixed-point implementation repeatedly scans the transition relation. Its worst-case cost is proportional to the number of iterations multiplied by the graph size. Worklist and SCC-based algorithms are planned for larger models.
