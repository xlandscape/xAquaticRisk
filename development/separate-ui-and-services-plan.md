## Plan: Split Prep And Analysis Services

Create a clean boundary between preparation/monitoring (parameterisation, run execution tracking, scenario slicing) and analysis/exploration (post-run PEC/GUTS outputs, tables, maps), while preserving current user behavior in phase 1 through compatibility endpoints and unchanged UI flows.

**Steps**
1. Phase 1 - Boundary definition and compatibility contract.
2. Define and freeze a versioned contract for shared artifacts and IDs: run_id format, MC path structure, scenario metadata payload, analysis job payload, and output manifest schema. This blocks all later steps.
3. Add explicit scenario metadata handoff so analysis does not depend on user.xml parsing fallback chains. Keep fallback reads temporarily for backward compatibility and add warnings when used. Depends on step 2.
4. Add a configuration contract for roots and runtime ownership (run_root, scenario_root, analysis_output_root, analysis runtime path/service URL) with strict startup validation and health endpoints. Depends on step 2.
5. Phase 2 - Internal modular split in current repo (no behavior change).
6. Extract control panel responsibilities into prep/monitoring module and analysis-client module within current server package. Keep existing routes stable; route handlers become thin adapters. Depends on steps 3-4.
7. Move shared normalization/path utilities into a shared core module used by both sides (reach ID normalization, canonical path resolution, run/scenario identifiers). Depends on step 6.
8. Introduce feature flags: local analysis mode (current subprocess) and remote analysis mode (new service URL) so operators can switch without UI changes. Depends on step 6.
9. Phase 3 - Standalone analysis service.
10. Create a standalone analysis API service exposing job start/status/outputs/table/file and optional geometry/timeseries exploration endpoints; start by reusing existing analysis script invocation and output conventions. Depends on steps 2-8.
11. Implement secure job workspace rules and artifact serving rules in analysis service (allowlist output files, normalize paths, deny traversal). Depends on step 10.
12. Update control panel analysis endpoints into a compatibility proxy that forwards to analysis service in remote mode and preserves response shapes. Depends on steps 8-11.
13. Phase 4 - Migrate scenario slicing ownership to prep service boundary.
14. Keep scenario slicing entirely in prep/monitoring service and remove any implicit analysis-side scenario mutation assumptions. Add explicit scenario inspection/subset contract documentation and progress-state schema. Parallel with step 12 after step 2.
15. Phase 5 - UI separation in control panel.
16. Split UI navigation and state stores into two domains: Prep & Monitoring and Analysis & Exploration. Keep existing tab names/flows initially; move to dedicated route namespaces and independent API clients under the hood. Depends on steps 6 and 12.
17. Add clear cross-linking entry points (e.g., from completed MC run in Monitor to Analyze action with prefilled payload), but no hidden automatic inference beyond explicit metadata. Depends on step 16.
18. Phase 6 - Hardening and deprecation.
19. Add deprecation telemetry/logging for compatibility fallbacks (user.xml scenario inference, local subprocess mode assumptions), publish timeline, then remove fallbacks in a major version. Depends on steps 10-17.

**Relevant files**
- c:/LocalWork/xAquaticRisk/controlpanel/server.py — main route surface and current coupling points: run discovery, scenario metadata inference, analysis job start/status/output APIs, map explorer endpoints.
- c:/LocalWork/xAquaticRisk/controlpanel/index.html — current UI tabs and client-side API usage that will be split into prep/monitoring and analysis/exploration domains.
- c:/LocalWork/xAquaticRisk/controlpanel/STATUS_AND_MANAGEMENT.md — operational contract location for runtime ownership, service mode, health checks, and migration notes.
- c:/LocalWork/xAquaticRisk/analysis/run_basic_analysis.py — canonical analysis execution entrypoint to wrap behind standalone analysis API.
- c:/LocalWork/xAquaticRisk/analysis/basic_analysis_common.py — shared scenario/exposure defaults and normalization helpers to extract into shared core contract.
- c:/LocalWork/xAquaticRisk/analysis/generate_field_ranking.py — field-level ranking capability that should stay fully in analysis domain.
- c:/LocalWork/xAquaticRisk/analysis/plot_field_ranking_map.py — analysis visualization pipeline retained in analysis service.
- c:/LocalWork/xAquaticRisk/analysis/plot_field_ranking_map_interactive.py — interactive map generation retained in analysis service.
- c:/LocalWork/xAquaticRisk/controlpanel/step2_pandas_backend.py — prep-side scenario slicing acceleration logic that remains in prep/monitoring service.

**Verification**
1. Contract tests: assert stable request/response schemas for existing control panel endpoints and new analysis service endpoints, including backward-compatible proxy behavior.
2. End-to-end prep flow test: load template, save parameters, launch run, monitor logs/status, abort/delete behavior unchanged.
3. End-to-end analysis flow test in local and remote modes: start analysis, poll status, list outputs, fetch table/file artifacts; verify identical payload shape to baseline.
4. Scenario slicing regression tests: subset creation (sync/async), cancel behavior, reach/time window correctness, and geometry-hydrology consistency checks.
5. Runtime portability checks: verify prep runtime and analysis runtime ownership separately, including missing dependency diagnostics and health endpoints.
6. UI regression checks: all existing user journeys still function; new domain split does not break deep links, run selection, or analyze-from-monitor handoff.

**Decisions**
- Chosen target architecture: full service split.
- Constraint: first phase must be backward compatible for existing users and automation.
- Included scope: parameterisation, simulation execution monitoring, scenario slicing, analysis execution/orchestration, analysis output exploration.
- Excluded scope for initial migration: changes to underlying model computational algorithms and result semantics.

**Further Considerations**
1. API ownership choice: Option A keep analysis API within same repository first (faster), Option B separate repository/service immediately (cleaner governance).
2. Map Explorer ownership: Option A keep in prep service (minimal change), Option B move to analysis service with geometry/timeseries APIs (clean domain boundary).
3. Deployment model: Option A shared filesystem mount between services, Option B artifact copy/sync contract (safer isolation, higher complexity).
