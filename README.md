# ARGUS

**An explicit-state CTL model checker written in OCaml.**

ARGUS models finite transition systems and evaluates Computation Tree Logic (CTL) formulas using fixed-point algorithms over state sets. It includes a formula parser, transition-system validation, a command-line interface, and regression tests.

## Capabilities

- Finite Kripke-style transition systems
- State labels and multiple initial states
- Model validation: duplicate IDs, missing successors and invalid initial references
- Propositional logic: NOT, AND, OR and implication
- CTL operators: EX, AX, EF, AF, EG, AG, EU and AU
- Least and greatest fixed-point evaluation
- Satisfying-state sets and initial-state verdict
- Shortest BFS witness paths for EF and EU
- BFS counterexample prefixes for failed AG properties
- Formula parser and model-file CLI
- Dune build, tests and GitHub Actions CI

## Model format

    # Traffic controller example
    initial 0
    state 0 ready -> 1 2
    state 1 safe -> 1
    state 2 unsafe -> 2

Each state line contains a state ID, zero or more atomic labels, an arrow, and zero or more successor IDs. Initial states are declared separately.

## Run

Requirements: OCaml 5.2 (4.14+ supported by package metadata) and Dune 3.11+.

    opam install . --deps-only --with-test -y
    dune build
    dune runtest

Check whether every initial state satisfies a CTL formula:

    dune exec argus -- examples/traffic.model "AG (safe -> EF safe)"

The CLI prints the formula, all satisfying states, and whether the formula holds in every initial state.

## CTL operators

| Operator | Meaning |
|---|---|
| EX p | Some immediate successor satisfies p |
| AX p | Every immediate successor satisfies p |
| EF p | There exists a path that eventually reaches p |
| AF p | Every path eventually reaches p |
| EG p | There exists a path that globally satisfies p |
| AG p | Every path globally satisfies p |
| E[p U q] | Some path satisfies p until q |
| A[p U q] | Every path satisfies p until q |

## Architecture

    Model text -> validated transition system
    Formula text -> CTL AST
    CTL AST + model -> fixed-point evaluator
    Result -> satisfying state set + initial verdict

## Semantics and limitations

ARGUS currently uses explicit-state set operations and assumes finite models. Universal predecessor checks use the standard vacuous-truth interpretation for states with no successors. If total Kripke structures are required by an application, add explicit deadlock validation or self-loop completion before checking.

Witness and counterexample path extraction, fairness constraints, symbolic BDD-based exploration, and temporal logic extensions are planned, not yet implemented.

See ROADMAP.md and docs/semantics.md.

## License

MIT.

Shortest witness and counterexample path search uses a FIFO queue with parent tracking, avoiding repeated list concatenation as the state graph grows.

The current trace extractor returns shortest finite prefixes for EF/EU witnesses and AG counterexamples. It does not yet return lasso-shaped infinite-path witnesses for EG/AF failures, nor does it implement fairness constraints.

