# convert_xrun.py

`convert_xrun.py` is a **migration utility for model users** who have `.xrun` parameterisation files created with an older version of xAquaticRisk. It upgrades them to the current format so they can be used with the present model version without manual editing.

---

## When do you need it?

You need `convert_xrun.py` if you have `.xrun` files from an older xAquaticRisk version that contain any of:

- A `<CropStage>` element (the old name for `<RautmannClass>`)
- The `<Control>` section at the **bottom** of the file instead of at the top
- Old parameter names such as `<SimID>`, `<DeleteFoldersAtFinish>`, `<Project>`, or `<ReachSelection>`
- A `<SimulationInfo>` section (old name for `<Control>`)

If you open such a file in the current Control Panel or pass it to `__start__.bat`, it will either be rejected by the schema validator or produce unexpected results. Run the converter first.

---

## What it produces

For each input file the script writes two output files:

| Output | Description |
|--------|-------------|
| `.xrun` (new format) | Updated XML file, schema-compliant, `<Control>` first, `<RautmannClass>` in place of `<CropStage>` |
| `.yaml` | Exact YAML equivalent of the converted `.xrun`, ready for use with `__start__.bat` or the Control Panel |

---

## Quick usage

Run from the model root using the model-core Python runtime:

```bat
model\core\bin\python-3.9.7-amd64\python.exe  convert_xrun.py  <old_file.xrun>
```

By default the outputs are written alongside the input file, **overwriting** the original `.xrun` and adding a new `.yaml`. To keep the original safe, use `--suffix`:

```bat
REM writes old_file_new.xrun and old_file_new.yaml, leaves old_file.xrun untouched
model\core\bin\python-3.9.7-amd64\python.exe  convert_xrun.py  old_file.xrun  --suffix _new
```

Redirect output to a different folder with `--outdir`:

```bat
model\core\bin\python-3.9.7-amd64\python.exe  convert_xrun.py  old_file.xrun  --outdir converted\
```

---

## Full reference

For the complete list of command-line options, conversion rules, and format details, see the dedicated reference page:

**[xrun Converter reference](../../xrun-converter.md)**
