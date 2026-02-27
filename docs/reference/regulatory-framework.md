# Regulatory Framework for Higher-Tier Aquatic Risk Assessment

This page summarises the regulatory and regulatory-scientific framework that underpins higher-tier,
landscape-level aquatic risk assessment for plant protection products (PPPs) in the European Union.

---

## 1. Core EU Legislation

### Placing PPPs on the Market

| Regulation | What it anchors |
|---|---|
| **Regulation (EC) No 1107/2009** | Approval of active substances and authorisation of products; protection goals and decision criteria (*no unacceptable effects on the environment, including aquatic ecosystems*). |
| **Regulation (EU) No 283/2013** | Data requirements for **active substances**, including fate, exposure and ecotoxicology packages that higher-tier work must build on. |
| **Regulation (EU) No 284/2013** | Data requirements for **PPPs** (formulated products). |
| **Regulation (EU) No 546/2011** | **Uniform Principles** for evaluation and authorisation — how Member States must interpret evidence and apply tiering / weight-of-evidence. |

### Use-Pattern and Mitigation Context

| Directive | What it anchors |
|---|---|
| **Directive 2009/128/EC** (Sustainable Use of Pesticides) | IPM, risk-reduction, drift/runoff mitigation expectations — often used to justify realistic mitigation scenarios in higher-tier assessments. |

### Water Policy Interface

| Directive | What it anchors |
|---|---|
| **Water Framework Directive 2000/60/EC** | Broader surface-water protection objectives, monitoring logic, and context for vulnerable catchments. |
| **EQS Directive 2008/105/EC** | Environmental Quality Standards for priority substances. |
| **Groundwater Directive 2006/118/EC** | Groundwater protection thresholds. |

!!! note
    The WFD family is not the PPP authorisation test itself, but is frequently referenced when arguing
    landscape relevance and aligning monitoring endpoints.

---

## 2. EFSA PPR Guidance — the Aquatic Tiered Approach

### Aquatic Organisms in Edge-of-Field Surface Waters

The **EFSA PPR Guidance on tiered risk assessment for aquatic organisms (edge-of-field surface
waters)** is the primary EU scientific guidance for aquatic RA tiering — from standard laboratory
tiers to higher-tier refinements — including how to use exposure models, effect endpoints and
refinements consistently.

### Specific Protection Goals (SPGs)

**EFSA Scientific Opinions on Specific Protection Goals (PPR Panel)** translate *"protect aquatic
ecosystems"* into operational protection goals defined by:

- **Entity** — what to protect (e.g., populations of aquatic invertebrates)
- **Attribute** — what property matters (e.g., abundance, biomass)
- **Magnitude** — how much change is tolerable
- **Temporal and spatial scale** — over which dimensions

These SPGs provide the conceptual bridge needed to justify landscape-level endpoints such as
population recovery or spatial refugia — concepts central to xAquaticRisk.

---

## 3. Exposure Modelling Framework

### FOCUS Surface Water

The **FOCUS surface water guidance and scenario reports** form the standardised exposure modelling
system used across the EU tiered scheme. FOCUS provides stepwise tiers for ditch, stream and pond
scenarios, covering drift, runoff, erosion and mitigation options.

**Connection to landscape-level work:** Landscape models such as xAquaticRisk need to demonstrate
consistency with — or justified deviation from — FOCUS assumptions, and show how spatial
heterogeneity, connectivity and mitigation are represented.

### Model Quality and Reporting

The **EFSA guidance on Good Modelling Practice (GMP)** sets expectations for documentation,
verification, sensitivity/uncertainty analysis and reproducibility. These requirements are critical
when introducing non-standard landscape models into the regulatory workflow.

---

## 4. Higher-Tier Effects Assessment

### Mesocosm Studies

EFSA/PPR recommendations on higher-tier aquatic effect studies (including mesocosm interpretation and
ecological relevance) define how to interpret community-level endpoints, recovery and ecological
relevance — often the *effects half* paired with refined exposure.

### Recovery and Ecological Relevance

PPR Panel scientific opinions on **recovery in aquatic organisms** and ecological threshold concepts
provide the logic for:

- when recovery-based arguments are acceptable,
- what *"acceptable effect"* means, and
- how spatial and temporal scales matter.

These are directly relevant to xAquaticRisk's landscape-level modelling claims.

### TKTD and Mechanistic Effect Modelling

EFSA/PPR opinions and guidance on **TKTD approaches (e.g., GUTS)** provide the regulatory-scientific
basis for using mechanistic models to extrapolate across time-variable exposure profiles — exactly
the situation when landscape models such as xAquaticRisk generate pulsed, spatially varying
concentrations.

xAquaticRisk implements GUTS-SD (Stochastic Death) and GUTS-IT (Individual Tolerance) via its
[LEffectModel](https://github.com/xlandscape/LEffectModel-Component) component.

---

## 5. Cross-Cutting EFSA Guidance

Several additional EFSA guidance documents are needed to make a landscape approach *"regulatory
grade"*:

| Guidance | Role |
|---|---|
| **Uncertainty Analysis** | Required structure for identifying, characterising and communicating uncertainty — essential when moving beyond standard tiers. |
| **Weight of Evidence (WoE)** | How to integrate multiple lines (FOCUS, monitoring, landscape exposure, mesocosms, population models) into a coherent regulatory argument. |
| **Use of Open Literature** | Rules for systematic identification and reliability assessment of published studies — often needed for landscape ecology parameters, species traits, dispersal data, etc. |

---

## 6. The Uniform Principles in Detail

The **Uniform Principles** (Regulation (EU) No 546/2011) are legally binding rules that all Member
States must follow when evaluating and authorising PPPs under Regulation (EC) No 1107/2009.

### Core Obligations for Member States

1. **Context of representative uses** — assessment must reflect realistic worst-case conditions for
   the proposed uses; Member States cannot assume more favourable conditions than supported by data.
2. **Scientific validity** — all studies must follow accepted test guidelines, be GLP-compliant where
   required, be relevant for the representative uses and be internally and externally consistent.
3. **Binding protection goals** — no harmful effects on human/animal health; no unacceptable effects
   on non-target organisms; no unacceptable groundwater contamination.

### Tiered Risk Assessment under the Uniform Principles

```
Tier 1   Conservative screening
  │      Standard lab endpoints, default assumptions, conservative exposure models.
  │      If acceptable → no higher tier needed.
  │      If unacceptable → consider refinement (do NOT automatically reject).
  ▼
Tier 2+  Refinement (not relaxation)
         • Refined exposure modelling (landscape-based, drift-reduction, mitigation)
         • Semi-field / field studies
         • Population-level or recovery-based evidence
         • Monitoring data
```

Higher-tier refinements must be:

- scientifically valid and relevant,
- representative of the proposed uses,
- not introducing unjustified assumptions, and
- not contradicting protection goals.

### Mitigation Measures

Member States may accept mitigation (buffer zones, drift-reduction technology, timing restrictions)
only if the measure is:

- effective and supported by evidence,
- enforceable and realistic in the Member State, and
- ensuring that the refined risk meets protection goals.

### Weight-of-Evidence Requirements

| Requirement | Detail |
|---|---|
| Structured, not discretionary | Member States must consider **all** relevant data, evaluate reliability/relevance/consistency, identify uncertainties explicitly, and give more weight to robust, representative and conservative evidence. |
| Resolving conflicting evidence | If Tier 1 indicates risk but a higher-tier study suggests safety, MS must verify that the higher-tier study is representative, covers relevant exposure pathways, has sufficient statistical power and does not contradict established mechanisms. |
| Precautionary resolution | If uncertainties remain and cannot be resolved by WoE, the PPP **cannot be authorised**. |

### Practical Rules

- **No compensation across protection goals** — a PPP cannot be authorised if it fails any single
  protection goal, even if other endpoints look favourable.
- **No reliance on unvalidated models** — higher-tier modelling must be validated and appropriate for
  the EU context.
- **Recovery arguments under strict conditions** — recovery must occur within the ecosystem context
  of the representative use, without relying on unrealistic immigration from untreated areas, and
  without violating the *"no unacceptable effects"* requirement.
- **Transparent documentation** — the authorisation report must show how each tier was applied, how
  WoE was used, how uncertainties were resolved and why the final conclusion meets the Uniform
  Principles.

---

## 7. How the Pieces Fit Together

The diagram below shows how the different regulatory elements combine in a landscape-level
higher-tier submission:

| Aspect | Regulatory basis |
|---|---|
| **Legal admissibility** | Regulation 1107/2009 + Uniform Principles (546/2011) define what must be protected and how decisions are made. |
| **Tiering logic** | EFSA PPR aquatic guidance defines the tiered pathway and what counts as an acceptable refinement. |
| **Exposure credibility** | FOCUS is the baseline; EFSA modelling guidance governs how departures toward landscape realism are justified and documented. |
| **Effects credibility** | EFSA/PPR opinions on SPGs, recovery, mesocosms and TKTD/population models provide scientific legitimacy for landscape-scale effect interpretation. |
| **Decision robustness** | EFSA uncertainty + weight-of-evidence guidance makes the whole package defensible under scrutiny. |

---

## 8. Relevance to xAquaticRisk

xAquaticRisk is designed to operate within this regulatory framework:

- **Tiered exposure refinement** — CascadeToxswa and CmfContinuous go beyond standard FOCUS by
  simulating fate in a spatially explicit hydrological network, while maintaining consistency with
  FOCUS principles.
- **TKTD effect modelling** — LEffectModel implements GUTS-SD/IT as recommended by EFSA/PPR for
  time-variable exposure extrapolation.
- **Population-level effects** — StreamCom models population dynamics of *Asellus aquaticus*,
  supporting recovery and recolonisation arguments within the SPG framework.
- **Landscape realism** — spatially explicit scenarios (field layout, stream network, buffer zones)
  capture the heterogeneity and connectivity required by higher-tier regulatory science.
- **Uncertainty characterisation** — Monte Carlo capabilities allow systematic exploration of
  parameter and scenario uncertainty, as required by EFSA uncertainty guidance.
- **Transparent reporting** — the ReportingObserver and AnalysisObserver provide standardised,
  reproducible outputs aligned with Good Modelling Practice expectations.
