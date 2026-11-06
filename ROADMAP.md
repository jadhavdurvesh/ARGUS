# ARGUS roadmap

## M0 — Transition systems
- [x] State representation and labels
- [x] Multiple initial states
- [x] Successor graph
- [x] Duplicate and dangling-reference validation
- [x] Model-file input format

## M1 — CTL engine
- [x] Propositional formulas
- [x] EX and AX predecessor operators
- [x] EF and AF least fixed points
- [x] EG and AG greatest fixed points
- [x] EU and AU until operators
- [x] Satisfying-state set
- [x] Initial-state verdict

## M2 — Usability and correctness
- [x] Formula tokenizer and parser
- [x] Command-line model checker
- [x] Regression tests for CTL operators
- [x] CI build and test workflow
- [ ] Witness path extraction
- [ ] Counterexample traces
- [ ] Source locations in formula diagnostics
- [ ] Property-based graph generation

## M3 — Advanced verification
- [ ] Fairness constraints
- [ ] Deadlock policies and total-model validation
- [ ] Strongly connected component optimizations
- [ ] Bounded model checking
- [ ] LTL-to-Buchi automata pipeline
- [ ] State-space reduction and symmetry
- [ ] Symbolic BDD backend
