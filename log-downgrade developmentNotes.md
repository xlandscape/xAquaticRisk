# Log Downgrade Development Notes

## Objective

Reduce monitor noise by downgrading all `WARN` log messages to `NOTE`, while keeping the process transparent in both backend and UI.

## Scope

- Backend parsing and severity reclassification in `controlpanel/server.py`
- Rule exposure via API endpoint
- UI visibility in monitor log controls in `controlpanel/index.html`

## Investigation Summary

### Code Areas Reviewed

- `controlpanel/server.py`
  - `_SEV_RE`
  - `_parse_log_lines`
  - `do_GET` routing

- `controlpanel/index.html`
  - log controls (`filterSev`, `loadLog`)
  - monitor rendering (`buildDetailFull`, `updateDetailInPlace`)

### Log Analysis Method

Used PowerShell to aggregate warning messages across MC logs:

```powershell
Get-ChildItem -Path run -Recurse -Filter 'mc_*.log' |
  Select-String -Pattern '^WARN\s+(.*)$' |
  ForEach-Object { $_.Matches[0].Groups[1].Value } |
  Group-Object |
  Sort-Object Count -Descending |
  Select-Object -First 40 |
  ForEach-Object { '{0,5} | {1}' -f $_.Count, $_.Name }
```

Top repetitive warnings identified included:

- `X3dfStore:SetValues`
- `*:ScalesChecker:GetValues`
- `... currently does not check the identity of ...`
- `Component relies on insensible high precision of z-coordinate`

Decision: to simplify operations and avoid maintaining exception lists, warning handling was expanded to a global policy.

## Implemented Changes

### Backend (`controlpanel/server.py`)

1. Added warning downgrade policy (`_WARN_TO_NOTE_RULES`).
2. Added helper functions:
   - `_warning_downgrade_rule(msg)`
   - `get_warning_downgrade_rules()`
3. Updated `_parse_log_lines()`:
   - Reclassifies all original `WARN` entries to `NOTE`
   - Adds metadata:
     - `reclassified_from: WARN`
     - `reclassification_rule: all_warn_to_note`
4. Added targeted non-fatal error reclassification:
   - `Failed to compute min/max, no valid pixels found in sampling. (GDAL error 1)`
   - reclassified from `ERROR` to `WARN`

5. Added endpoint:
   - `GET /api/log-warning-downgrades`

### Frontend (`controlpanel/index.html`)

1. Added monitor UI section (collapsible) in log controls:
   - `WARN downgraded to NOTE`
2. Loads rules from backend and renders them in the monitor view.

## Downgrade Rules

### WARN -> NOTE

1. `all_warn_to_note`
   - Pattern: `^.*$`
   - Effect: every original `WARN` entry is shown as `NOTE`

### ERROR -> WARN

1. `gdal_minmax_no_valid_pixels`
   - Pattern: `^Failed to compute min/max, no valid pixels found in sampling\. \(GDAL error 1\)$`
   - Effect: this specific non-fatal GDAL error is shown as `WARN`

## Validation

### Static Checks

- No editor-reported errors in:
  - `controlpanel/server.py`
  - `controlpanel/index.html`

### Compile Check

```powershell
& 'model/core/bin/python-3.9.7-amd64/python.exe' -m py_compile 'controlpanel/server.py'
```

Result: `server.py compile OK`

### Rule Exposure Check

```powershell
& 'model/core/bin/python-3.9.7-amd64/python.exe' -c "import sys; sys.path.insert(0, r'controlpanel'); import server, json; print(json.dumps({'rules': server.get_warning_downgrade_rules()}, ensure_ascii=False, indent=2))"
```

Result: expected JSON list of all configured downgrade rules.

## Operational Notes

- Changes affect monitor parsing/presentation in the control panel.
- Raw log files are not rewritten.
- To apply updates in the UI:
  1. Restart control panel server
  2. Hard-refresh browser (`Ctrl+F5`)

## Follow-Up (Optional)

- Make rules configurable via external JSON/YAML
- Add toggle in UI to disable/enable downgrade layer
- Add per-run statistics: `downgraded_warn_count`
