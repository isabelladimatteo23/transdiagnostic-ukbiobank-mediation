Repository: Analytic code for UKBB transdiagnostic anhedonia mediation analysis

Contents
- UKBB_ModeratedMediation.R : Analysis script used to produce the manuscript results.
- UKBB_DescriptiveStats.R: Analysis script used to produce descriptive statistics.
- 01_get_dx.ipynb: Extract and organize diagnosis variables from UKB data
- 02_filter_unmatched.ipynb: Filter out unmatched subjects before matching
- 03_psm_matching.ipynb: Perform propensity score matching
- 04_create_HiTOP_flags.ipynb: Generate HiTOP flag variables for the dataset

Data access
- Raw data are UK Biobank and are not included. To reproduce the analysis you must obtain UKB access and place the CSV (named ukbb_full_matched_hitop_clean_0825.csv) in your Downloads folder or data/ directory as described in the script.
- Project ID: [your UKB project ID here]

UKBB_DescriptiveStats.R: 
- Performs descriptive statistics on the UKBB matched dataset (`ukbb_full_matched_hitop_clean_0825.csv`), pre-submission (11/18/25).
- Categorical variables (`sex`, `int`, `ext`, `psy`) are summarized as n (%) for patients (`dx == 1`) and controls (`dx != 1`), with chi-square tests and Cramér's V effect sizes.
- Continuous variables (`age`, `tdep`, `pal`, `freq_unenthusiasm_disinterest`, `freq_social_visits`) are summarized as mean (SD) for patients and controls, with Mann-Whitney U tests and rank-biserial correlations.
- Histograms and density plots, as well as QQ-plots, are generated for continuous variables to assess distribution.

UKBB_ModeratedMediation.R:
- The script re-runs the manuscript analyses (bootstrap = 2000) without setting a seed by default to match the original run. Bootstrap results can vary slightly on rerun unless a seed is used.
- The output CSVs in output/ contain the parameter tables (estimates, SEs, p-values, percentile 95% CIs) used to check manuscript numbers.

Notebooks:
- The notebooks contain intermediate steps, preprocessing, and dataset generation to facilitate reproducibility and further exploration.
