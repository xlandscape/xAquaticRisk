# xLandscape Design Evaluation and Improvement Plan

## Context

This note elaborates on the statement that xLandscape is not using a modern DAG engine like Airflow, Prefect, or Dask.

The point is not that xLandscape is fundamentally weak. It is that xLandscape implements orchestration concerns itself, while modern workflow platforms provide many orchestration capabilities out of the box.

## What the comparison means in practice

Modern workflow engines typically provide:

1. Native task graph semantics.
2. Built-in retries, backoff, caching, and checkpointing.
3. Rich run-state observability and lineage.
4. Fine-grained scheduling with explicit resource constraints.
5. Better resumability and failure isolation.

xLandscape currently covers these concerns through custom framework logic in core classes, XML composition, observers, and wrapper-specific subprocess handling.

## Strengths to preserve in xLandscape

xLandscape already has a key strength many generic engines do not enforce by default: domain-aware semantics.

Current strengths include:

- explicit scales (time/hour, time/day, space/reach, etc.)
- physical units
- offsets and element naming metadata
- geometry-aware data contracts
- model-variant composability via configuration

A good modernization path should preserve these semantics.

## Design gaps and improvement opportunities

### 1. Execution is order-driven, not graph-native

Current behavior:

- dependencies are described by FromOutput links
- execution remains sequential by component order in mc.xml

Risks:

- hidden dependency mistakes are discovered late
- independent branches cannot run concurrently
- graph validation is limited

Improvements:

1. Build an explicit dependency graph from composition links.
2. Perform topological sorting before execution.
3. Detect cycles and unresolved dependencies up front.
4. Allow parallel execution of independent components.

### 2. Resumability is limited at component granularity

Current behavior:

- some store re-use exists
- no full task lifecycle persistence across failures

Risks:

- expensive reruns after partial failure
- weak restart ergonomics for long scenarios

Improvements:

1. Persist per-component state (pending/running/succeeded/failed/skipped/cached).
2. Add deterministic input fingerprints for cache reuse.
3. Support resume-from-failure with dependency-consistent invalidation.

### 3. Retry policy is mostly component-local

Current behavior:

- external wrappers call subprocesses, but retry behavior is not uniformly managed by the framework

Risks:

- transient errors cause unnecessary hard failures
- retry behavior inconsistent across components

Improvements:

1. Add framework-level retry policy configuration.
2. Support backoff and retry budgets.
3. Classify retryable vs non-retryable failures.

### 4. Observability is logging-focused, not state-focused

Current behavior:

- observers capture messages and logs
- limited standardized event model for runtime analytics

Risks:

- difficult to inspect critical path and bottlenecks
- troubleshooting relies heavily on text logs

Improvements:

1. Emit structured lifecycle events (JSON).
2. Track timings, retries, resource usage, and failure causes per component.
3. Expose run DAG and status in control panel views.

### 5. Resource management is coarse

Current behavior:

- MC-level parallelism is configurable
- component-level resource declarations are not first-class

Risks:

- oversubscription and unstable performance
- poor utilization on larger machines

Improvements:

1. Add component resource declarations (cpu, memory, io, external runtime slots).
2. Schedule work with resource-aware admission.
3. Separate executor backends (local serial, local parallel, future distributed).

### 6. Configuration model is powerful but hard to verify early

Current behavior:

- macro substitution and XML are flexible
- many checks happen at runtime

Risks:

- runtime failures from config typos or type mismatches
- steep learning curve for developers

Improvements:

1. Add a compile/validate stage for experiment and MC configs.
2. Validate types, scales, unit compatibility, macro resolution, and dependency completeness pre-run.
3. Provide precise error diagnostics with source-location hints.

### 7. Wrapper implementation burden is high

Current behavior:

- wrappers repeatedly implement staging, conversion, subprocess calls, output parsing, metadata remapping

Risks:

- duplication and drift in implementation quality
- harder maintenance across many components

Improvements:

1. Introduce a component adapter SDK with reusable primitives.
2. Standardize subprocess execution contracts.
3. Add wrapper contract tests and golden-data integration tests.

### 8. Data backend abstraction can be strengthened

Current behavior:

- X3df store carries rich semantics and works well for current model coupling

Risks:

- backend flexibility and ecosystem integration are constrained

Improvements:

1. Define a logical data API independent of one storage backend.
2. Keep semantics first; allow backend evolution behind the API.
3. Improve selective-read and metadata indexing paths for large outputs.

## Implementation roadmap

### Phase 1: low-risk, high-value

1. Add graph compile and validation mode (cycles, unresolved links, type/scale checks).
2. Add structured lifecycle events in core execution.
3. Introduce uniform retry/backoff policies at framework level.

Expected impact:

- immediate developer feedback
- better failure diagnostics
- improved run robustness

### Phase 2: execution engine upgrade

1. Implement dependency-aware scheduler (topological execution).
2. Add safe intra-MC parallel execution for independent components.
3. Persist component state for resume/restart.

Expected impact:

- runtime speedups on complex compositions
- improved failure recovery

### Phase 3: platform hardening

1. Build component SDK and templates for wrappers.
2. Expand control panel with graph/status/timeline views.
3. Add contract and integration test suites for wrappers and data contracts.

Expected impact:

- lower maintenance cost
- higher consistency and quality
- easier onboarding for new component development

## Practical architectural guidance

The best path is likely evolutionary, not replacement.

Recommended strategy:

1. Keep xLandscape domain semantics and component interfaces.
2. Modernize orchestration internals (graph model, scheduler, state store, retries).
3. Preserve backward compatibility for existing mc.xml and wrappers where possible.

This approach improves orchestration quality while protecting the domain-specific strengths xLandscape already has.

## One-line summary

xLandscape should keep its semantic data-contract strengths and incrementally adopt modern workflow-engine concepts: explicit DAG execution, stateful resumability, structured observability, and resource-aware scheduling.