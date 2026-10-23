# Inputs and execution boundaries

## Keep data outside the public repository

Use an authorized local project directory with the original structure. No data
files are supplied with this source snapshot. The primary scripts read:

```text
data/derived/hrs_multistate_analytic.rds
data/derived/charls_multistate_analytic.rds
```

Additional analyses also read both cohorts' files with suffixes:

```text
_person_wave_scored.rds
_transition_intervals_scored.rds
```

HRS participant IDs are stored in `hhidpn`; CHARLS participant IDs are stored
in `ID`. Rows represent either person-waves or adjacent-wave intervals,
depending on the file. Do not interpret repeated intervals as unique people.

## Core models from existing cleaned data

Run the following in Ubuntu after setting the environment variable
`PROJECT_ROOT` to an authorized local project directory. The commands create a
fresh temporary output directory and read existing data through a symbolic
link. They do not prepare or clean source data.

```bash
git clone https://github.com/cx295410-dot/adapted-hbcs-stroke-analysis.git
cd adapted-hbcs-stroke-analysis
CODE_ROOT="$(pwd)"
: "${PROJECT_ROOT:?Set PROJECT_ROOT to your authorized local project directory}"
RUN_ROOT="$(mktemp -d)"
mkdir "$RUN_ROOT/results"
ln -s "$PROJECT_ROOT/data" "$RUN_ROOT/data"
(
  cd "$RUN_ROOT"
  Rscript "$CODE_ROOT/code/09_primary_transition_models.R"
  Rscript "$CODE_ROOT/code/10_sensitivity_models.R"
  Rscript "$CODE_ROOT/code/11_descriptive_and_category_models.R"
)
```

Install the required CRAN packages in your own R library before running. This
repository publication did not execute these commands or refit the models.

Do not run the preparation or BMI-cleaning scripts against this symbolic link:
some of those scripts overwrite derived files. Preparation requires an isolated
working copy of the original cohort inputs.

## Additional scripts

The additional scripts take two positional arguments: the local project root
and a separate output directory. `00_preflight.R` describes local file schemas
and package availability. Its output is for local use, not automatic upload.

`01_additional_analyses.R` contains the original supplementary modules and
generates intermediate and aggregate outputs. Some CHARLS models in this
historical script had sparse education categories. Stable supplementary CHARLS
models are specified in `02_stable_charls_and_ipw.R`, which uses joint wave and
education strata and needs `ipw_target_for_bootstrap.rds` in its output directory.
That intermediate is participant-level data and is intentionally excluded here.

Do not assume that running the historical scripts sequentially constitutes a
validated final pipeline. If a sparse-model failure interrupts an earlier script,
the required intermediate may not have been created. No automatic workaround or
result substitution is applied in this snapshot.

`03_flow_and_education.R` produces flow counts and education-category summaries.
`04_redraw_figures.R` reads aggregate results from the local project and additional
output directory. Historical aggregate dependencies, such as spline predictions
and the targeted component-summary audit, are not bundled with this code-only
snapshot.

## Final-release checks

For a future end-to-end reproduction pipeline, add the missing historical
modules, reconcile the per-SD denominators, record package versions, and verify
the execution sequence against the manuscript. Death-reclassification and full
complete-score selection weighting are not included in this archive. Keep raw
and derived participant records private throughout.
