# Intermittent template_beek7 MC Error

## Summary

The intermittent message

`ERROR MC run completed with errors`

followed by

`Error in x$.self$finalize() : attempt to apply non-function`

is very likely caused by an old bundled R package bug in the spray-drift step, not by the `template_beek7` parameter values themselves.

## Why this is the likely cause

1. `template_beek7` normally runs the SprayDrift component.
   - In [parameterisation/template_beek7.yaml](../parameterisation/template_beek7.yaml), `DepositionInputFile` is empty.
   - The comment in that file says SprayDrift is only skipped if a deposition file is provided.

2. The spray-drift step launches an embedded R script that loads `terra`.
   - [model/variant/XSprayDrift/SprayDrift.py](../model/variant/XSprayDrift/SprayDrift.py) launches the bundled `Rscript.exe` and `SDModel_XSprayDrift_x3df_2.R`.
   - [model/variant/XSprayDrift/module/SDModel_XSprayDrift_x3df_2.R](../model/variant/XSprayDrift/module/SDModel_XSprayDrift_x3df_2.R) loads `terra`.

3. The bundled package versions are old and match a known upstream issue.
   - [model/variant/XSprayDrift/module/R-4.1.2/library/terra/DESCRIPTION](../model/variant/XSprayDrift/module/R-4.1.2/library/terra/DESCRIPTION) shows `terra` version `1.4-20`.
   - [model/variant/XSprayDrift/module/R-4.1.2/library/Rcpp/DESCRIPTION](../model/variant/XSprayDrift/module/R-4.1.2/library/Rcpp/DESCRIPTION) shows `Rcpp` version `1.0.7`.

4. The exact error text is already documented by `terra` as a garbage-collection/finalizer issue.
   - [analysis/lib/terra/NEWS.md](../analysis/lib/terra/NEWS.md) says:
     - `The annoying garbage collection messages Error in x$.self$finalize() : attempt to apply non-function is now suppressed in most cases.`
     - It further notes that the issue should go away with a newer `Rcpp`.

## Why it only happens sometimes

This fits a garbage-collection timing problem.

- The error appears when certain `terra` objects are finalized during R garbage collection.
- Garbage collection timing depends on memory state and object lifetime.
- Therefore the same simulation can complete cleanly in one run and emit this message in another.

That explains why the problem is intermittent and not deterministic for `template_beek7`.

## Why xAquaticRisk reports it as an MC error

xAquaticRisk currently promotes any raw subprocess output line containing the word `error` to a run error.

- [model/core/base/Observer.py](../model/core/base/Observer.py) checks raw text with a regex matching `error`.
- The same observer later emits `MC run completed with errors` when such a line was seen.

So a benign third-party package finalizer message can be classified as a model error even if the actual spray-drift computation finished successfully.

## Interpretation

This is most likely a false-positive run error classification, not evidence that the simulation logic itself failed.

If the spray-drift outputs and downstream results are complete, the message is probably harmless package noise from `terra` finalization.

## Likely fixes

1. Update the embedded `XSprayDrift` R library stack, especially `terra` and `Rcpp`.
2. Filter this exact known `terra` finalizer message in [model/core/base/Observer.py](../model/core/base/Observer.py) so it does not count as a fatal MC error.

## Practical conclusion

The cause is very likely the bundled `terra` plus `Rcpp` combination used by SprayDrift, together with xAquaticRisk's current log classification rule, not a stochastic bug in the Beek7 parameterisation itself.