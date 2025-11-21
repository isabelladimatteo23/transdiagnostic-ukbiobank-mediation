```text
Repository: UK Biobank moderated-mediation analyses

Contents
- UKBB_ModeratedMediation.R : Analysis script used to produce the manuscript results (does not include raw data).

Data access
- Raw data are UK Biobank and are not included. To reproduce the analysis you must obtain UKB access and place the CSV (named ukbb_full_matched_hitop_clean_0825.csv) in your Downloads folder or data/ directory as described in the script.
- Project ID: [your UKB project ID here]

Notes about reproducibility
- The script re-runs the manuscript analyses (bootstrap = 2000) without setting a seed by default to match the original run. Bootstrap results can vary slightly on rerun unless a seed is used.
- The output CSVs in output/ contain the parameter tables (estimates, SEs, p-values, percentile 95% CIs) used to check manuscript numbers.
```
