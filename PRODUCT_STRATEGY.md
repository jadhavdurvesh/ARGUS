# ARGUS — Independent Product Strategy

## Product identity
ARGUS is a general-purpose explicit-state formal verification toolkit for finite transition systems and temporal properties. Its initial engine supports CTL model checking.

## Product boundary
- **Core:** transition-system representation, model validation, property/formula language, model-checking algorithms, witnesses/counterexamples, and result reporting.
- **Adapters:** input formats, CI integrations, visualization, and domain-specific model generators.
- **Applications:** standalone verification examples and reusable domain models.

The core must not hard-code FireBot states, robot modes, or safety assumptions. FireBot models are application fixtures built on top of generic ARGUS semantics.

## Independent use cases
1. Verify workflow/state-machine correctness in business applications.
2. Check protocol and distributed-service state machines.
3. Explore scheduling, access-control, and resource-allocation models.

## Product requirements
- Precise documented semantics, including deadlocks and universal quantification.
- Reproducible verdicts with witness and counterexample traces.
- Source-located diagnostics and a stable model/formula format.
- Property-based model generation and algorithm regression tests.
- Explicit limits for finite-state exploration and resource usage.
- Optional CI output with machine-readable verdicts and trace artifacts.

## Non-goals
ARGUS does not prove that a model faithfully represents the real system, validate physical hardware, or certify safety. Verification results are only as sound and complete as the model, assumptions, and implementation.

## FireBot adapter
FireBot-specific state machines and invariants live in a separate model package/fixture set. They consume ARGUS without adding FireBot concepts to the engine.

## Readiness gates
- [ ] Documented formal semantics and deadlock policy.
- [ ] Trace generation for supported temporal operators.
- [ ] Property-based tests and model-size limits.
- [ ] Two non-FireBot model suites.
- [ ] Reproducible CLI/CI reports and independent release process.
