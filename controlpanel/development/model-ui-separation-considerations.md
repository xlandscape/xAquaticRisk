# Model-UI Separation: Architectural Considerations

## Current State

xAquaticRisk is organized as a monorepo containing:

- **`model/`** — LandscapeModel-Core + component submodules (XSprayDrift, StreamCom, LP50, etc.)
- **`controlpanel/`** — Python web UI (Flask/Flask-RESTful backend)
- **`analysis/`** — Jupyter notebooks, R scripts, reporting observers
- Each tier has bundled portable Python runtimes

## Benefits of Separation

### 1. Independence & Reusability
The model becomes a standalone package that other tools, scripts, or UIs can consume without the controlpanel overhead. Researchers could use just the model CLI or via a Python API without deploying the web interface.

### 2. Independent Release Cycles
Model fixes/updates don't force controlpanel releases, and vice versa. You could fix a UI bug without touching model version tags, and ship model improvements without waiting for UI validation.

### 3. Clearer API Boundaries
Model repo would have an explicit public API contract. Currently the relationship is implicit—controlpanel knows all the internals, making it harder for external users to understand what's stable.

### 4. Improved Xcopy-Readiness
A separate model runtime is easier to distribute as a portable package or containerize. The controlpanel becomes an optional thin wrapper, improving portability and reducing deployment complexity.

### 5. Faster CI/CD Cycles
Separate test suites don't block each other. A failing integration test in controlpanel doesn't delay model component merges, and vice versa.

---

## Tradeoffs & Challenges

### 1. Increased Complexity
Managing N repositories + version alignment + integration tests is harder than a single monorepo. Onboarding contributors becomes more involved.

### 2. Reproducibility & Version Coordination
Right now, a single repo commit + submodule commits guarantees a reproducible state. Split repos require explicit tagging, release coordination, and dependency management to ensure matching versions work together.

### 3. Distribution & Bundling Logic
You'd need explicit packaging/downloading logic to ship both model and controlpanel together, or decide on a split distribution strategy. More moving parts to maintain.

### 4. Scenario & Data Bundling
Currently, scenarios are submodules bundled with the main model repo. If separated, scenarios need versioning and dependency management as well—adding another dimension of complexity.

---

## Recommended Approach: Hybrid Model

**Start with a phased hybrid separation** rather than a hard fork:

### Phase 1: Extract Model to Separate Repo
- Create `xAquaticRisk-Model` repo (or rename current `xAquaticRisk`)
- Move `model/`, `analysis/` to model repo
- Keep model components + scenarios as submodules within
- Publish model as a PyPI package or GitHub release artifact

### Phase 2: Make Controlpanel a Consumer
- Controlpanel stays in a separate repo or branch
- Depends on `xAquaticRisk-Model` via pip/git submodule
- Controlpanel ships with model as optional bundled asset
- Can be installed standalone or from main xAquaticRisk bundle repo

### Phase 3: Integration Testing
- Add integration tests that verify controlpanel + latest model work together
- Tag releases in coordination: `xAquaticRisk-Model@2.88` + `xAquaticRisk-ControlPanel@2.88` = `xAquaticRisk-Bundle@2.88`

### Phase 4: Optional: Full Separation
- If successful, move controlpanel to completely separate org/repo
- Model becomes a true upstream dependency
- Bundles are assembled on demand

---

## Advantages of Hybrid Approach

✅ Model independence for third-party tools  
✅ Cleaner separation of concerns  
✅ Single reproducible release artifact  
✅ Backward compatibility during transition  
✅ Minimal overhead vs. full separation  
✗ Still some coordination needed (manageable)

---

## Key Questions to Answer Before Proceeding

1. **Who uses what?** Are there external users of the model alone, or is controlpanel always deployed together?
2. **Release cadence** — How often do model vs. UI changes occur independently?
3. **Development velocity** — Would splitting repos slow down or speed up your team?
4. **Distribution strategy** — Do you plan to publish the model as a library, or always as a bundle?
5. **Scenario management** — Should scenarios be versioned separately or remain tied to model releases?

---

## Implementation Checklist (for future reference)

- [ ] Document model API (what's stable, what's internal)
- [ ] Create setup.py / pyproject.toml for model packaging
- [ ] Add integration tests between model and controlpanel
- [ ] Set up coordinated tagging/release workflow
- [ ] Decide on pip/git dependency management
- [ ] Test xcopy-readiness of separated model
- [ ] Migrate docs to reflect dual-use scenarios (standalone model vs. bundled)
