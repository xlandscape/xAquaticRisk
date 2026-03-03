"""Rewrites dashboard/index.html with the two-tab layout."""
import os

HTML = """\
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>xAquaticRisk \u2014 Dashboard</title>
<style>
/* \u2500\u2500 Reset & base \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500 */
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
:root{
  --bg:#0f1117;--bg2:#181b24;--bg3:#21252f;--border:#2c313d;
  --text:#c9d1d9;--text2:#8b949e;--accent:#58a6ff;
  --ok:#3fb950;--info:#c9d1d9;--note:#79c0ff;--warn:#d29922;--err:#f85149;
  --font:'Segoe UI','Inter',system-ui,sans-serif;
  --mono:'Cascadia Code','Fira Code','Consolas',monospace;
}
html{font-size:14px}
body{font-family:var(--font);background:var(--bg);color:var(--text);line-height:1.5;height:100vh;display:flex;flex-direction:column;overflow:hidden}

/* \u2500\u2500 App shell \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500 */
.header{background:var(--bg2);border-bottom:1px solid var(--border);padding:.8rem 1.5rem;display:flex;align-items:center;gap:1rem;flex-shrink:0}
.header h1{font-size:1.15rem;font-weight:600;color:#fff}
.header .tag{font-size:.7rem;background:var(--accent);color:#000;padding:2px 8px;border-radius:3px;font-weight:600}
.header .refresh-info{margin-left:auto;font-size:.75rem;color:var(--text2)}

/* \u2500\u2500 Top-level tabs \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500 */
.top-tabs{background:var(--bg2);border-bottom:1px solid var(--border);display:flex;gap:0;padding:0 1.5rem;flex-shrink:0}
.top-tab{padding:.55rem 1.2rem;font-size:.82rem;font-weight:600;cursor:pointer;color:var(--text2);border-bottom:2px solid transparent;transition:all .15s;user-select:none}
.top-tab:hover{color:var(--text)}
.top-tab.active{color:var(--accent);border-bottom-color:var(--accent)}

/* \u2500\u2500 Tab panels \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500 */
.tab-panel{flex:1;display:none;overflow:hidden}
.tab-panel.active{display:flex;flex-direction:column}

/* \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
   PARAMETERISATION TAB
\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550 */
.param-page{flex:1;overflow-y:auto;padding:1.2rem 1.5rem}
.param-section{background:var(--bg2);border:1px solid var(--border);border-radius:6px;margin-bottom:.9rem;overflow:hidden}
.param-section-head{padding:.6rem 1rem;font-weight:600;font-size:.82rem;cursor:pointer;display:flex;justify-content:space-between;align-items:center;user-select:none;transition:background .15s}
.param-section-head:hover{background:var(--bg3)}
.param-section-head .arrow{font-size:.7rem;transition:transform .2s}
.param-section-head.collapsed .arrow{transform:rotate(-90deg)}
.param-section-body{padding:.9rem 1rem;border-top:1px solid var(--border)}
.param-section-body.collapsed{display:none}
.form-row{display:grid;grid-template-columns:1fr 1fr;gap:.9rem}
@media(max-width:700px){.form-row{grid-template-columns:1fr}}
.form-group{margin-bottom:.75rem}
.form-group:last-child{margin-bottom:0}
.form-group label{display:block;font-weight:600;font-size:.78rem;color:var(--text);margin-bottom:.35rem}
.form-group input,.form-group select,.form-group textarea{
  width:100%;padding:.45rem .7rem;background:var(--bg);color:var(--text);
  border:1px solid var(--border);border-radius:4px;font-size:.82rem;font-family:var(--font);
  transition:border-color .2s,box-shadow .2s}
.form-group input:focus,.form-group select:focus,.form-group textarea:focus{
  outline:none;border-color:var(--accent);box-shadow:0 0 0 2px rgba(88,166,255,.2)}
.form-group select option{background:var(--bg2)}
.help-text{font-size:.72rem;color:var(--text2);margin-top:.25rem}
.input-row{display:flex;gap:.5rem}
.input-row input,.input-row select{flex:1}
.param-sub-heading{font-size:.78rem;font-weight:700;color:var(--accent);margin:1rem 0 .6rem 0;padding-bottom:.3rem;border-bottom:1px solid var(--border)}
.param-actions{background:var(--bg2);border-top:1px solid var(--border);padding:.75rem 1.5rem;display:flex;gap:.7rem;align-items:center;flex-wrap:wrap;flex-shrink:0}
.btn{padding:.45rem 1rem;border:none;border-radius:4px;font-size:.8rem;font-weight:600;cursor:pointer;transition:all .2s;display:inline-flex;align-items:center;gap:.4rem}
.btn-primary{background:var(--ok);color:#000}
.btn-primary:hover{filter:brightness(1.15)}
.btn-secondary{background:var(--accent);color:#000}
.btn-secondary:hover{filter:brightness(1.15)}
.btn-outline{background:transparent;color:var(--text);border:1px solid var(--border)}
.btn-outline:hover{background:var(--bg3)}
.status-msg{font-size:.78rem;padding:.3rem .7rem;border-radius:4px;display:none}
.status-msg.success{display:inline-block;background:rgba(63,185,80,.15);color:var(--ok);border:1px solid rgba(63,185,80,.3)}
.status-msg.error{display:inline-block;background:rgba(248,81,73,.15);color:var(--err);border:1px solid rgba(248,81,73,.3)}
.spinner-inline{width:16px;height:16px;border:2px solid var(--border);border-top-color:var(--accent);border-radius:50%;animation:spin 1s linear infinite;display:none}
.spinner-inline.show{display:inline-block}
@keyframes spin{to{transform:rotate(360deg)}}

/* \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
   RUN MONITORING TAB
\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550 */
.monitor-container{display:flex;flex:1;overflow:hidden}
.sidebar{width:320px;min-width:240px;background:var(--bg2);border-right:1px solid var(--border);overflow-y:auto;flex-shrink:0}
.main{flex:1;overflow-y:auto;padding:1.2rem 1.5rem}
.sidebar-header{padding:.7rem 1rem;font-size:.8rem;font-weight:600;color:var(--text2);text-transform:uppercase;letter-spacing:.05em;border-bottom:1px solid var(--border)}
.run-card{padding:.65rem 1rem;border-bottom:1px solid var(--border);cursor:pointer;transition:background .15s}
.run-card:hover{background:var(--bg3)}
.run-card.active{background:var(--bg3);border-left:3px solid var(--accent)}
.run-card .name{font-weight:600;font-size:.85rem;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.run-card .meta{font-size:.72rem;color:var(--text2);display:flex;gap:.8rem;margin-top:2px}
.run-card .status-dot{display:inline-block;width:8px;height:8px;border-radius:50%;margin-right:4px;vertical-align:middle}
.status-running{background:var(--accent);animation:pulse 1.5s infinite}
.status-finished{background:var(--ok)}
.status-warning{background:var(--warn)}
.status-error{background:var(--err)}
.status-initializing{background:var(--text2);animation:pulse 1.5s infinite}
.status-unknown{background:var(--text2)}
@keyframes pulse{0%,100%{opacity:1}50%{opacity:.4}}
.panel{background:var(--bg2);border:1px solid var(--border);border-radius:6px;margin-bottom:1rem}
.panel-head{padding:.55rem 1rem;font-weight:600;font-size:.82rem;border-bottom:1px solid var(--border);display:flex;align-items:center;gap:.6rem}
.panel-body{padding:.8rem 1rem}
.overview-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:.7rem;margin-bottom:1rem}
.stat-card{background:var(--bg3);border:1px solid var(--border);border-radius:6px;padding:.7rem .9rem;text-align:center}
.stat-card .label{font-size:.7rem;color:var(--text2);text-transform:uppercase;letter-spacing:.04em}
.stat-card .value{font-size:1.5rem;font-weight:700;margin-top:.15rem}
.stat-card .value.ok{color:var(--ok)}.stat-card .value.warn{color:var(--warn)}.stat-card .value.err{color:var(--err)}.stat-card .value.info{color:var(--accent)}
.mc-row{display:flex;align-items:center;gap:.8rem;margin-bottom:.5rem}
.mc-label{width:180px;font-size:.78rem;font-family:var(--mono);white-space:nowrap;overflow:hidden;text-overflow:ellipsis;flex-shrink:0}
.progress-bar{flex:1;height:18px;background:var(--bg);border-radius:3px;overflow:hidden}
.progress-fill{height:100%;border-radius:3px;transition:width .5s ease}
.progress-fill.running{background:linear-gradient(90deg,var(--accent),#388bfd);animation:shimmer 2s infinite}
.progress-fill.finished{background:var(--ok)}.progress-fill.warning{background:var(--warn)}.progress-fill.error{background:var(--err)}
.progress-pct{width:48px;font-size:.75rem;text-align:right;color:var(--text2);flex-shrink:0}
.mc-comp{font-size:.7rem;color:var(--text2);margin-left:190px;margin-top:-4px;margin-bottom:6px}
@keyframes shimmer{0%{opacity:.85}50%{opacity:1}100%{opacity:.85}}
.log-tabs{display:flex;gap:0;border-bottom:1px solid var(--border);padding:0 .5rem}
.log-tab{padding:.4rem .9rem;font-size:.75rem;cursor:pointer;color:var(--text2);border-bottom:2px solid transparent;transition:all .15s}
.log-tab:hover{color:var(--text)}.log-tab.active{color:var(--accent);border-bottom-color:var(--accent)}
.log-controls{display:flex;align-items:center;gap:.7rem;padding:.4rem 1rem;border-bottom:1px solid var(--border)}
.log-controls label{font-size:.72rem;color:var(--text2)}
.log-controls input[type=checkbox]{accent-color:var(--accent)}
.log-controls select{font-size:.72rem;background:var(--bg);color:var(--text);border:1px solid var(--border);border-radius:3px;padding:2px 6px}
.log-output{font-family:var(--mono);font-size:.78rem;line-height:1.65;max-height:55vh;overflow-y:auto;padding:.6rem 1rem;background:var(--bg);white-space:pre-wrap;word-break:break-all}
.log-line{padding:1px 0}.log-line .sev{display:inline-block;width:46px;font-weight:700}
.sev-ERROR{color:var(--err)}.sev-WARN{color:var(--warn)}.sev-NOTE{color:var(--note)}.sev-OK{color:var(--ok)}.sev-INFO{color:var(--info)}
.log-line .detail{color:var(--text2);padding-left:46px;display:block}
.params-table{width:100%;border-collapse:collapse;font-size:.78rem}
.params-table th{text-align:left;padding:.35rem .6rem;color:var(--text2);border-bottom:1px solid var(--border);font-weight:600}
.params-table td{padding:.3rem .6rem;border-bottom:1px solid var(--border)}
.params-table .sec{color:var(--accent);font-weight:600}.params-table .param{font-family:var(--mono);color:var(--text)}.params-table .val{font-family:var(--mono);color:var(--ok)}
.empty{text-align:center;padding:4rem 2rem;color:var(--text2)}.empty h2{font-size:1.1rem;margin-bottom:.5rem}.empty p{font-size:.85rem}
.comp-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(280px,1fr));gap:.3rem .8rem;font-size:.72rem;font-family:var(--mono)}
.comp-item{display:flex;align-items:center;gap:.4rem;padding:2px 4px;border-radius:3px}
.comp-item.done{color:var(--ok)}.comp-item.active{color:var(--accent);font-weight:600;background:rgba(88,166,255,.08)}.comp-item.pending{color:var(--text2);opacity:.4}
.comp-icon{width:14px;text-align:center;flex-shrink:0}
::-webkit-scrollbar{width:6px;height:6px}::-webkit-scrollbar-track{background:var(--bg)}::-webkit-scrollbar-thumb{background:var(--border);border-radius:3px}::-webkit-scrollbar-thumb:hover{background:var(--text2)}
</style>
</head>
<body>

<div class="header">
  <h1>xAquaticRisk</h1>
  <span class="tag">DASHBOARD</span>
  <span class="refresh-info" id="refreshInfo">Monitor auto-refresh: 3 s</span>
</div>

<div class="top-tabs">
  <div class="top-tab active" id="tabBtnParam" onclick="switchTab('param')">&#9881; Parameterisation</div>
  <div class="top-tab" id="tabBtnMonitor" onclick="switchTab('monitor')">&#128202; Run Monitoring</div>
</div>

<!-- PARAMETERISATION TAB -->
<div class="tab-panel active" id="panelParam">
<div class="param-page" id="paramPage">

  <div class="param-section">
    <div class="param-section-head" onclick="togglePS(this)"><span>xrun File</span><span class="arrow">&#9660;</span></div>
    <div class="param-section-body">
      <div class="form-group">
        <label>Path to xrun Files</label>
        <div class="input-row"><input type="text" id="xrunPath" placeholder="e.g., C:\\LocalWork\\xAquaticRisk">
        <button class="btn btn-secondary" onclick="loadXrunFiles()">Browse</button></div>
        <div class="help-text">Directory where xrun files are located</div>
      </div>
      <div class="form-group">
        <label>Open xrun File</label>
        <div class="input-row"><select id="xrunSelect"><option value="">Select an xrun file\u2026</option></select>
        <button class="btn btn-outline" onclick="openXrunFile()">Load</button></div>
        <div class="help-text">Select an existing xrun file to load its parameters</div>
      </div>
      <div class="form-group">
        <label>Save Parameterisation As</label>
        <div class="input-row"><input type="text" id="saveAsName" placeholder="e.g., MyParameterisation">
        <button class="btn btn-outline" onclick="saveAsXrun()">Save As</button></div>
        <div class="help-text">Save current parameters with a custom filename</div>
      </div>
    </div>
  </div>

  <div class="param-section">
    <div class="param-section-head" onclick="togglePS(this)"><span>Experiment Configuration</span><span class="arrow">&#9660;</span></div>
    <div class="param-section-body">
      <div class="form-group">
        <label>Experiment ID</label>
        <input type="text" id="Control/ExperimentID" placeholder="e.g., Test_Run_aqRisk">
        <div class="help-text">Unique identifier for this simulation run</div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>Number of Monte Carlo Runs</label>
          <input type="number" step="1" min="1" id="Control/NumberMC" placeholder="e.g., 1">
          <div class="help-text">Total Monte Carlo iterations</div></div>
        <div class="form-group"><label>Parallel Processes</label>
          <input type="number" step="1" min="1" id="Control/NumberParallelProcesses" placeholder="e.g., 2">
          <div class="help-text">Simultaneous MC runs</div></div>
      </div>
      <div class="form-group"><label>Scenario *</label>
        <select id="Scenario/LandscapeScenario"><option value="">Loading scenarios\u2026</option></select>
        <div class="help-text">Landscape scenario to use</div></div>
      <div class="form-row">
        <div class="form-group"><label>Simulation Start Date</label>
          <input type="text" id="Scenario/SimulationStart" placeholder="YYYY-MM-DD">
          <div class="help-text">First day of simulation</div></div>
        <div class="form-group"><label>Simulation End Date</label>
          <input type="text" id="Scenario/SimulationEnd" placeholder="YYYY-MM-DD">
          <div class="help-text">Last day of simulation</div></div>
      </div>
    </div>
  </div>

  <div class="param-section">
    <div class="param-section-head" onclick="togglePS(this)"><span>PPP Use (Application)</span><span class="arrow">&#9660;</span></div>
    <div class="param-section-body">
      <div class="form-row">
        <div class="form-group"><label>Application Rate (g/ha)</label>
          <input type="number" step="any" id="PppUse/ApplicationRate" placeholder="e.g., 12.5">
          <div class="help-text">Rate applied to all applications</div></div>
        <div class="form-group"><label>Application Time Window</label>
          <input type="text" id="PppUse/ApplicationTimeWindow" placeholder="MM-DD to MM-DD">
          <div class="help-text">e.g., 04-07 to 04-21</div></div>
      </div>
    </div>
  </div>

  <div class="param-section">
    <div class="param-section-head" onclick="togglePS(this)"><span>Mitigation Measures</span><span class="arrow">&#9660;</span></div>
    <div class="param-section-body">
      <div class="form-row">
        <div class="form-group"><label>In-Crop Buffer (m)</label>
          <input type="number" step="any" id="Mitigation/InCropBuffer" placeholder="e.g., 0">
          <div class="help-text">In-crop buffer not applied (meters)</div></div>
        <div class="form-group"><label>Technology Drift Reduction (0\u20131)</label>
          <input type="number" step="0.01" min="0" max="1" id="Mitigation/TechnologyDriftReduction" placeholder="e.g., 0">
          <div class="help-text">Fraction of drift filtered out</div></div>
      </div>
    </div>
  </div>

  <div class="param-section">
    <div class="param-section-head" onclick="togglePS(this)"><span>Exposure Settings</span><span class="arrow">&#9660;</span></div>
    <div class="param-section-body">
      <div class="form-row">
        <div class="form-group"><label>Rautmann Class</label>
          <select id="Exposure/RautmannClass">
            <option value="orchards.early">Orchards Early</option>
            <option value="orchards.late">Orchards Late</option>
            <option value="arable">Arable</option>
          </select>
          <div class="help-text">Drift class used in simulation</div></div>
        <div class="form-group"><label>Deposition Input File</label>
          <input type="text" id="Exposure/DepositionInputFile" placeholder="Leave empty for default">
          <div class="help-text">Optional CSV with predefined spray-drift depositions</div></div>
      </div>
    </div>
  </div>

  <div class="param-section">
    <div class="param-section-head collapsed" onclick="togglePS(this)"><span>Environmental Fate</span><span class="arrow">&#9660;</span></div>
    <div class="param-section-body collapsed">
      <div class="form-row">
        <div class="form-group"><label>Run StepsRiverNetwork</label>
          <select id="EnvironmentalFate/RunStepsRiverNetwork"><option value="true">Yes</option><option value="false">No</option></select></div>
        <div class="form-group"><label>Run CascadeToxswa</label>
          <select id="EnvironmentalFate/RunCascadeToxswa"><option value="true">Yes</option><option value="false">No</option></select></div>
      </div>
      <div class="param-sub-heading">Substance Properties</div>
      <div class="form-row">
        <div class="form-group"><label>Molar Mass (g/mol)</label><input type="number" step="any" id="EnvironmentalFate/MolarMass"></div>
        <div class="form-group"><label>Solubility in Water (mg/l)</label><input type="number" step="any" id="EnvironmentalFate/SolubilityInWater"></div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>DT50 Water (days)</label><input type="number" step="any" id="EnvironmentalFate/DT50sw"><div class="help-text">Half-life in water at 20\u00b0C</div></div>
        <div class="form-group"><label>DT50 Sediment (days)</label><input type="number" step="any" id="EnvironmentalFate/DT50sed"><div class="help-text">Half-life in sediment at 20\u00b0C</div></div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>KOC</label><input type="number" step="any" id="EnvironmentalFate/KOC"><div class="help-text">Organic carbon-water partition coefficient</div></div>
        <div class="form-group"><label>Diffusion Coefficient (m\u00b2/d)</label><input type="text" id="EnvironmentalFate/DiffusionCoefficient"></div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>Saturated Vapour Pressure (Pa)</label><input type="text" id="EnvironmentalFate/SaturatedVapourPressure"><div class="help-text">At 20\u00b0C</div></div>
        <div class="form-group"><label>Molar Enthalpy of Vaporization (kJ/mol)</label><input type="number" step="any" id="EnvironmentalFate/MolarEnthalpyOfVaporization"></div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>Molar Enthalpy of Dissolution (kJ/mol)</label><input type="number" step="any" id="EnvironmentalFate/MolarEnthalpyOfDissolution"></div>
        <div class="form-group"><label>Reference Concentration for KOC (mg/l)</label><input type="number" step="any" id="EnvironmentalFate/ReferenceConcentrationForKOC"></div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>Molar Activation Enthalpy in Water (kJ/mol)</label><input type="number" step="any" id="EnvironmentalFate/MolarActivationEnthalpyOfTransformationInWater"></div>
        <div class="form-group"><label>Molar Activation Enthalpy in Sediment (kJ/mol)</label><input type="number" step="any" id="EnvironmentalFate/MolarActivationEnthalpyOfTransformationInSediment"></div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>Freundlich Exponent</label><input type="number" step="any" id="EnvironmentalFate/FreundlichExponentInSedimentAndSuspendedParticles"><div class="help-text">In sediment and suspended particles</div></div>
        <div class="form-group"><label>Adsorption on Macrophytes (l/kg)</label><input type="number" step="any" id="EnvironmentalFate/CoefficientForLinearAdsorptionOnMacrophytes"></div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>Threshold SW</label><input type="number" step="any" id="EnvironmentalFate/ThresholdSW"><div class="help-text">Surface water precision (StepsRiverNetwork)</div></div>
        <div class="form-group"><label>Threshold Sediment</label><input type="number" step="any" id="EnvironmentalFate/ThresholdSediment"><div class="help-text">Sediment precision (StepsRiverNetwork)</div></div>
      </div>
    </div>
  </div>

  <div class="param-section">
    <div class="param-section-head collapsed" onclick="togglePS(this)"><span>Effect Modelling</span><span class="arrow">&#9660;</span></div>
    <div class="param-section-body collapsed">
      <div class="form-row">
        <div class="form-group"><label>Run LGuts</label>
          <select id="Effects/RunLGuts"><option value="true">Yes</option><option value="false">No</option></select>
          <div class="help-text">Simulate effects via LGuts</div></div>
        <div class="form-group"><label>Warm-up Years</label><input type="number" step="1" min="0" id="Effects/NumberOfWarmUpYears"><div class="help-text">Years for stable population cycles</div></div>
      </div>
      <div class="form-group"><label>Recovery Period (years)</label><input type="number" step="1" min="0" id="Effects/RecoveryPeriodYears"><div class="help-text">Years after last application</div></div>
      <div class="param-sub-heading">Species 1</div>
      <div class="form-group"><label>Species 1 Name</label><input type="text" id="Effects/Species1" placeholder="e.g., Asellus aquaticus"></div>
      <div class="form-row">
        <div class="form-group"><label>SD Dominant Rate Constant (1/h)</label><input type="number" step="any" id="Effects/Species1DominantRateConstantSD"></div>
        <div class="form-group"><label>SD Threshold Concentration (ng/l)</label><input type="number" step="any" id="Effects/Species1ThresholdConcentrationSD"></div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>SD Killing Rate (l/(ng\u00b7h))</label><input type="number" step="any" id="Effects/Species1KillingRateSD"></div>
        <div class="form-group"><label>IT Dominant Rate Constant (1/h)</label><input type="number" step="any" id="Effects/Species1DominantRateConstantIT"></div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>IT Threshold Distribution (ng/l)</label><input type="number" step="any" id="Effects/Species1ThresholdDistributionIT"></div>
        <div class="form-group"><label>IT Width of Threshold Distribution</label><input type="number" step="any" id="Effects/Species1WidthOfThresholdDistributionIT"></div>
      </div>
      <div class="param-sub-heading">Species 2</div>
      <div class="form-group"><label>Species 2 Name</label><input type="text" id="Effects/Species2" placeholder="e.g., Cloeon dipterum"></div>
      <div class="form-row">
        <div class="form-group"><label>SD Dominant Rate Constant (1/h)</label><input type="number" step="any" id="Effects/Species2DominantRateConstantSD"></div>
        <div class="form-group"><label>SD Threshold Concentration (ng/l)</label><input type="number" step="any" id="Effects/Species2ThresholdConcentrationSD"></div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>SD Killing Rate (l/(ng\u00b7h))</label><input type="number" step="any" id="Effects/Species2KillingRateSD"></div>
        <div class="form-group"><label>IT Dominant Rate Constant (1/h)</label><input type="number" step="any" id="Effects/Species2DominantRateConstantIT"></div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>IT Threshold Distribution (ng/l)</label><input type="number" step="any" id="Effects/Species2ThresholdDistributionIT"></div>
        <div class="form-group"><label>IT Width of Threshold Distribution</label><input type="number" step="any" id="Effects/Species2WidthOfThresholdDistributionIT"></div>
      </div>
      <div class="param-sub-heading">Species 3</div>
      <div class="form-group"><label>Species 3 Name</label><input type="text" id="Effects/Species3" placeholder="e.g., Gammarus pulex"></div>
      <div class="form-row">
        <div class="form-group"><label>SD Dominant Rate Constant (1/h)</label><input type="number" step="any" id="Effects/Species3DominantRateConstantSD"></div>
        <div class="form-group"><label>SD Threshold Concentration (ng/l)</label><input type="number" step="any" id="Effects/Species3ThresholdConcentrationSD"></div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>SD Killing Rate (l/(ng\u00b7h))</label><input type="number" step="any" id="Effects/Species3KillingRateSD"></div>
        <div class="form-group"><label>IT Dominant Rate Constant (1/h)</label><input type="number" step="any" id="Effects/Species3DominantRateConstantIT"></div>
      </div>
      <div class="form-row">
        <div class="form-group"><label>IT Threshold Distribution (ng/l)</label><input type="number" step="any" id="Effects/Species3ThresholdDistributionIT"></div>
        <div class="form-group"><label>IT Width of Threshold Distribution</label><input type="number" step="any" id="Effects/Species3WidthOfThresholdDistributionIT"></div>
      </div>
    </div>
  </div>

  <div class="param-section" id="additionalParamsCard" style="display:none">
    <div class="param-section-head collapsed" onclick="togglePS(this)"><span>Additional Parameters</span><span class="arrow">&#9660;</span></div>
    <div class="param-section-body collapsed" id="additionalParams"></div>
  </div>

</div><!-- /param-page -->
<div class="param-actions">
  <button class="btn btn-outline" onclick="loadTemplate()">&#8635; Reset to Template</button>
  <button class="btn btn-secondary" onclick="saveConfiguration()">&#128190; Save Parameterisation</button>
  <button class="btn btn-primary" onclick="runSimulation()">&#9654; Run Simulation</button>
  <span class="spinner-inline" id="runSpinner"></span>
  <span class="status-msg" id="statusMsg"></span>
</div>
</div><!-- /panelParam -->

<!-- RUN MONITORING TAB -->
<div class="tab-panel" id="panelMonitor">
  <div class="monitor-container">
    <div class="sidebar">
      <div class="sidebar-header">Simulation Runs</div>
      <div id="runList"></div>
    </div>
    <div class="main" id="mainContent">
      <div class="empty">
        <h2>No run selected</h2>
        <p>Select a run from the sidebar to view its progress and logs.<br>
        The list refreshes automatically while simulations are running.</p>
      </div>
    </div>
  </div>
</div>

<script>
// ================================================================
// TAB SWITCHING
// ================================================================
function switchTab(name) {
  document.querySelectorAll('.top-tab').forEach(t => t.classList.remove('active'));
  document.querySelectorAll('.tab-panel').forEach(p => p.classList.remove('active'));
  const key = name.charAt(0).toUpperCase() + name.slice(1);
  document.getElementById('tabBtn' + key).classList.add('active');
  document.getElementById('panel' + key).classList.add('active');
}

// ================================================================
// PARAMETERISATION
// ================================================================
let templateParams = {};
let activeXrunPath = null;
let activeXrunFilename = null;

const knownParams = [
  'Scenario/LandscapeScenario','Scenario/SimulationStart','Scenario/SimulationEnd',
  'Control/ExperimentID','Control/NumberMC','Control/NumberParallelProcesses',
  'PppUse/ApplicationRate','PppUse/ApplicationTimeWindow',
  'Mitigation/InCropBuffer','Mitigation/TechnologyDriftReduction',
  'Exposure/RautmannClass','Exposure/DepositionInputFile',
  'EnvironmentalFate/RunStepsRiverNetwork','EnvironmentalFate/RunCascadeToxswa',
  'EnvironmentalFate/MolarMass','EnvironmentalFate/SolubilityInWater',
  'EnvironmentalFate/DT50sw','EnvironmentalFate/DT50sed',
  'EnvironmentalFate/KOC','EnvironmentalFate/DiffusionCoefficient',
  'EnvironmentalFate/SaturatedVapourPressure','EnvironmentalFate/MolarEnthalpyOfVaporization',
  'EnvironmentalFate/MolarEnthalpyOfDissolution','EnvironmentalFate/ReferenceConcentrationForKOC',
  'EnvironmentalFate/MolarActivationEnthalpyOfTransformationInWater',
  'EnvironmentalFate/MolarActivationEnthalpyOfTransformationInSediment',
  'EnvironmentalFate/FreundlichExponentInSedimentAndSuspendedParticles',
  'EnvironmentalFate/CoefficientForLinearAdsorptionOnMacrophytes',
  'EnvironmentalFate/ThresholdSW','EnvironmentalFate/ThresholdSediment',
  'Effects/RunLGuts','Effects/NumberOfWarmUpYears','Effects/RecoveryPeriodYears',
  'Effects/Species1','Effects/Species1DominantRateConstantSD','Effects/Species1ThresholdConcentrationSD',
  'Effects/Species1KillingRateSD','Effects/Species1DominantRateConstantIT','Effects/Species1ThresholdDistributionIT','Effects/Species1WidthOfThresholdDistributionIT',
  'Effects/Species2','Effects/Species2DominantRateConstantSD','Effects/Species2ThresholdConcentrationSD',
  'Effects/Species2KillingRateSD','Effects/Species2DominantRateConstantIT','Effects/Species2ThresholdDistributionIT','Effects/Species2WidthOfThresholdDistributionIT',
  'Effects/Species3','Effects/Species3DominantRateConstantSD','Effects/Species3ThresholdConcentrationSD',
  'Effects/Species3KillingRateSD','Effects/Species3DominantRateConstantIT','Effects/Species3ThresholdDistributionIT','Effects/Species3WidthOfThresholdDistributionIT'
];

function togglePS(head) {
  head.classList.toggle('collapsed');
  head.nextElementSibling.classList.toggle('collapsed');
}

function showStatus(msg, type) {
  const el = document.getElementById('statusMsg');
  el.textContent = msg;
  el.className = 'status-msg ' + type;
  if (type === 'success') setTimeout(() => { el.className = 'status-msg'; }, 5000);
}

function getFormData() {
  const params = {};
  const allKeys = [...new Set([...Object.keys(templateParams), ...knownParams])];
  for (const key of allKeys) {
    const el = document.getElementById(key);
    if (el && el.value !== '') params[key] = el.value;
  }
  return params;
}

async function loadScenarios() {
  try {
    const r = await fetch('/api/scenarios');
    const scenarios = await r.json();
    const sel = document.getElementById('Scenario/LandscapeScenario');
    sel.innerHTML = '<option value="">Select a scenario\u2026</option>';
    scenarios.forEach(s => {
      const o = document.createElement('option');
      o.value = s.path; o.textContent = s.name; sel.appendChild(o);
    });
  } catch(e) { console.error('Scenarios:', e); }
}

async function loadTemplate() {
  try {
    const r = await fetch('/api/template');
    templateParams = await r.json();
    for (const [key, info] of Object.entries(templateParams)) {
      const el = document.getElementById(key);
      if (el) el.value = info.value || '';
    }
    const div = document.getElementById('additionalParams');
    const card = document.getElementById('additionalParamsCard');
    div.innerHTML = '';
    let hasExtra = false;
    for (const [key, info] of Object.entries(templateParams)) {
      if (!knownParams.includes(key)) {
        hasExtra = true;
        const g = document.createElement('div'); g.className = 'form-group';
        const lbl = document.createElement('label'); lbl.textContent = info.tag || key;
        const inp = document.createElement('input'); inp.type = 'text'; inp.id = key; inp.value = info.value || '';
        g.appendChild(lbl); g.appendChild(inp);
        if (info.description) { const h = document.createElement('div'); h.className = 'help-text'; h.textContent = info.description; g.appendChild(h); }
        div.appendChild(g);
      }
    }
    card.style.display = hasExtra ? 'block' : 'none';
    showStatus('Template loaded', 'success');
  } catch(e) { showStatus('Error loading template: ' + e.message, 'error'); }
}

async function saveConfiguration() {
  const params = getFormData();
  try {
    const body = { parameters: params };
    if (activeXrunPath && activeXrunFilename) { body.path = activeXrunPath; body.filename = activeXrunFilename; }
    const r = await fetch('/api/save', { method:'POST', headers:{'Content-Type':'application/json'}, body:JSON.stringify(body) });
    const res = await r.json();
    showStatus(res.message, res.status === 'success' ? 'success' : 'error');
  } catch(e) { showStatus('Save error: ' + e.message, 'error'); }
}

async function runSimulation() {
  const params = getFormData();
  if (!params['Scenario/LandscapeScenario']) { showStatus('Please select a scenario', 'error'); return; }
  const spinner = document.getElementById('runSpinner');
  spinner.classList.add('show');
  try {
    const r = await fetch('/api/run', { method:'POST', headers:{'Content-Type':'application/json'}, body:JSON.stringify(params) });
    const res = await r.json();
    spinner.classList.remove('show');
    if (res.status === 'success') {
      showStatus(res.message, 'success');
      setTimeout(() => switchTab('monitor'), 1500);
    } else { showStatus(res.message, 'error'); }
  } catch(e) { spinner.classList.remove('show'); showStatus('Error: ' + e.message, 'error'); }
}

async function loadXrunFiles() {
  const path = document.getElementById('xrunPath').value.trim();
  if (!path) { showStatus('Enter a directory path first', 'error'); return; }
  try {
    const r = await fetch('/api/xrun-files', { method:'POST', headers:{'Content-Type':'application/json'}, body:JSON.stringify({path}) });
    const res = await r.json();
    if (res.status === 'success') {
      const sel = document.getElementById('xrunSelect');
      sel.innerHTML = '<option value="">Select an xrun file\u2026</option>';
      (res.files||[]).forEach(f => { const o = document.createElement('option'); o.value = f.name; o.textContent = f.name; sel.appendChild(o); });
      showStatus('Found ' + res.count + ' xrun file(s)', 'success');
    } else { showStatus(res.message, 'error'); }
  } catch(e) { showStatus('Browse error: ' + e.message, 'error'); }
}

async function openXrunFile() {
  const filename = document.getElementById('xrunSelect').value;
  const path = document.getElementById('xrunPath').value.trim();
  if (!filename) { showStatus('Select an xrun file first', 'error'); return; }
  if (!path) { showStatus('Enter a directory path first', 'error'); return; }
  try {
    const r = await fetch('/api/open-xrun', { method:'POST', headers:{'Content-Type':'application/json'}, body:JSON.stringify({filename, path}) });
    const res = await r.json();
    if (res.status === 'success') {
      for (const [key, val] of Object.entries(res.parameters)) { const el = document.getElementById(key); if (el) el.value = val; }
      showStatus('Loaded: ' + filename, 'success');
    } else { showStatus(res.message, 'error'); }
  } catch(e) { showStatus('Load error: ' + e.message, 'error'); }
}

async function saveAsXrun() {
  const filename = document.getElementById('saveAsName').value;
  const path = document.getElementById('xrunPath').value.trim();
  if (!filename) { showStatus('Enter a filename', 'error'); return; }
  if (!path) { showStatus('Enter a directory path', 'error'); return; }
  try {
    const r = await fetch('/api/save-as', { method:'POST', headers:{'Content-Type':'application/json'}, body:JSON.stringify({filename, path, parameters: getFormData()}) });
    const res = await r.json();
    if (res.status === 'success') {
      showStatus(res.message, 'success');
      activeXrunPath = path; activeXrunFilename = res.filename;
      document.getElementById('saveAsName').value = res.filename;
      loadXrunFiles();
    } else { showStatus(res.message, 'error'); }
  } catch(e) { showStatus('Save-as error: ' + e.message, 'error'); }
}

// ================================================================
// RUN MONITORING
// ================================================================
let selectedRun = null;
let activeLogTab = 'experiment.log';
let autoScroll = true;
let filterSev = 'ALL';
const refreshInterval = 3000;

async function apiGet(path) { const r = await fetch(path); return r.json(); }

async function loadRuns() {
  const runs = await apiGet('/api/runs');
  const el = document.getElementById('runList');
  if (!el) return;
  el.innerHTML = runs.map(r =>
    '<div class="run-card ' + (selectedRun===r.id?'active':'') + '" onclick="selectRun(\'' + r.id + '\')">' +
    '<div class="name"><span class="status-dot status-' + r.status + '"></span>' + r.id + '</div>' +
    '<div class="meta"><span>' + statusLabel(r.status) + '</span><span>MC: ' + r.mc_finished + '/' + (r.mc_total||'?') + '</span>' +
    (r.elapsed ? '<span>' + r.elapsed + '</span>' : '') + '</div></div>'
  ).join('') || '<div style="padding:1rem;font-size:.78rem;color:var(--text2)">No runs found in run/ folder.</div>';
}

function statusLabel(s) {
  return ({running:'Running',finished:'Finished',warning:'Warnings',error:'Errors',initializing:'Starting',unknown:'Unknown'})[s]||s;
}

async function selectRun(id) { selectedRun = id; await loadRuns(); await loadDetail(); }

async function loadDetail() {
  if (!selectedRun) return;
  const d = await apiGet('/api/runs/' + selectedRun);
  if (!d) { document.getElementById('mainContent').innerHTML = '<div class="empty"><h2>Run not found</h2></div>'; return; }
  const logTabs = ['experiment.log', ...(d.mc_runs||[]).map(mc => 'mc_' + mc.name + '.log')];
  if (!logTabs.includes(activeLogTab)) activeLogTab = logTabs[0];
  let html = '';

  html += '<div class="overview-grid">' +
    '<div class="stat-card"><div class="label">Status</div><div class="value ' + (d.status==='finished'?'ok':d.status==='error'?'err':d.status==='warning'?'warn':'info') + '">' + statusLabel(d.status) + '</div></div>' +
    '<div class="stat-card"><div class="label">Elapsed</div><div class="value info">' + (d.elapsed||'\u2014') + '</div></div>' +
    '<div class="stat-card"><div class="label">MC Runs</div><div class="value info">' + d.mc_runs.length + '</div></div>' +
    '<div class="stat-card"><div class="label">Errors</div><div class="value ' + (d.severity_counts.ERROR?'err':'ok') + '">' + d.severity_counts.ERROR + '</div></div>' +
    '<div class="stat-card"><div class="label">Warnings</div><div class="value ' + (d.severity_counts.WARN?'warn':'ok') + '">' + d.severity_counts.WARN + '</div></div>' +
    '</div>';

  if (d.mc_runs.length) {
    html += '<div class="panel"><div class="panel-head">Monte Carlo Progress</div><div class="panel-body">';
    for (const mc of d.mc_runs) {
      const pct = Math.round(mc.progress * 100);
      const cls = mc.status==='error'?'error':mc.status==='warning'?'warning':mc.status==='finished'?'finished':'running';
      html += '<div class="mc-row"><div class="mc-label" title="' + mc.name + '">' + mc.name + '</div>' +
        '<div class="progress-bar"><div class="progress-fill ' + cls + '" style="width:' + pct + '%"></div></div>' +
        '<div class="progress-pct">' + pct + '%</div></div>';
      if (mc.current_component) html += '<div class="mc-comp">&#9654; ' + mc.current_component + '</div>';
    }
    html += '</div></div>';
  }

  if (d.mc_runs.length) {
    const mc0 = d.mc_runs[0];
    const doneSet = new Set(mc0.components_done);
    html += '<div class="panel"><div class="panel-head">Component Pipeline \u2014 ' + mc0.name + '</div><div class="panel-body"><div class="comp-grid">';
    let foundActive = false;
    for (const c of (mc0.initialized||[])) {
      if (doneSet.has(c)) html += '<div class="comp-item done"><span class="comp-icon">&#10003;</span>' + c + '</div>';
      else if (!foundActive && mc0.current_component === c) { html += '<div class="comp-item active"><span class="comp-icon">&#9654;</span>' + c + '</div>'; foundActive=true; }
      else html += '<div class="comp-item pending"><span class="comp-icon">&#9679;</span>' + c + '</div>';
    }
    html += '</div></div></div>';
  }

  html += '<div class="panel"><div class="panel-head">Log Output</div>' +
    '<div class="log-tabs" id="logTabs">' + logTabs.map(t => '<div class="log-tab ' + (t===activeLogTab?'active':'') + '" onclick="switchLog(\'' + t + '\')">' + t.replace('.log','') + '</div>').join('') + '</div>' +
    '<div class="log-controls">' +
    '<label><input type="checkbox" id="chkAutoScroll" ' + (autoScroll?'checked':'') + ' onchange="autoScroll=this.checked"> Auto-scroll</label>' +
    '<label>Filter: <select id="selFilter" onchange="filterSev=this.value;loadLog()">' +
    ['ALL','ERROR','WARN','NOTE','OK','INFO'].map(v => '<option value="' + v + '" ' + (filterSev===v?'selected':'') + '>' + (v==='ALL'?'All':v) + '</option>').join('') +
    '</select></label></div>' +
    '<div class="log-output" id="logOutput">Loading\u2026</div></div>';

  if (d.parameters && Object.keys(d.parameters).length) {
    html += '<div class="panel"><div class="panel-head">Parameters</div><div class="panel-body">' +
      '<table class="params-table"><thead><tr><th>Section</th><th>Parameter</th><th>Value</th></tr></thead><tbody>';
    let lastSec = '';
    for (const [key, val] of Object.entries(d.parameters)) {
      const parts = key.split('/'); const sec = parts[0]; const param = parts.slice(1).join('/');
      html += '<tr><td class="sec">' + (sec!==lastSec?sec:'') + '</td><td class="param">' + param + '</td><td class="val">' + escHtml(val) + '</td></tr>';
      lastSec = sec;
    }
    html += '</tbody></table></div></div>';
  }

  document.getElementById('mainContent').innerHTML = html;
  await loadLog();
}

async function loadLog() {
  if (!selectedRun) return;
  const logEl = document.getElementById('logOutput');
  if (!logEl) return;
  const data = await apiGet('/api/runs/' + selectedRun + '/log/' + activeLogTab + '?tail=500');
  if (data.error) { logEl.innerHTML = '<span style="color:var(--warn)">' + data.error + '</span>'; return; }
  let entries = data.entries || [];
  if (filterSev !== 'ALL') entries = entries.filter(e => e.sev === filterSev);
  logEl.innerHTML = entries.map(e => {
    const lines = e.msg.split('\\n');
    const detail = lines.slice(1).map(l => '<span class="detail">' + escHtml(l) + '</span>').join('');
    return '<div class="log-line"><span class="sev sev-' + e.sev + '">' + e.sev.padEnd(5) + '</span> ' + escHtml(lines[0]) + detail + '</div>';
  }).join('');
  if (autoScroll) logEl.scrollTop = logEl.scrollHeight;
}

function switchLog(name) { activeLogTab = name; loadDetail(); }
function escHtml(s) { return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;'); }

async function tick() {
  try { await loadRuns(); if (selectedRun) await loadDetail(); } catch(e) { console.error(e); }
  setTimeout(tick, refreshInterval);
}

document.addEventListener('DOMContentLoaded', () => {
  loadScenarios();
  loadTemplate();
  tick();
});
</script>
</body>
</html>
"""

path = r'c:\LocalWork\xAquaticRisk\dashboard\index.html'
with open(path, 'w', encoding='utf-8') as f:
    f.write(HTML)
print(f"Written {len(HTML)} bytes, {HTML.count(chr(10))} lines")
