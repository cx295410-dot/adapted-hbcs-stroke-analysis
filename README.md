# Adapted hBCS and stroke and mortality transitions

Research code for longitudinal analyses of an adapted harmonized Brain Care
Score in the Health and Retirement Study (HRS) and the China Health and
Retirement Longitudinal Study (CHARLS).

**Research-code archive v1.0.0.** This repository preserves the existing core
and additional-analysis scripts for the study. The analysis-coverage map lists
the available modules and their input requirements. It is a source-code archive,
not a validated one-command reproduction pipeline.

Archived release: <https://doi.org/10.5281/zenodo.23602467> (version 1.0.0).

## Study

Longitudinal Adapted Brain Care Score and Stroke–Mortality Transitions in US
and Chinese Aging Cohorts: A Multistate Analysis

Authors, in manuscript order: Xiang Chen, Jianjun Wang, Junjie Liu, and Aibin Cheng.

The exposure is an adapted nine-domain score, not the original 21-point Brain
Care Score. HRS and CHARLS were analyzed separately. CHARLS provides independent
cross-cohort replication. The analyses estimate associations, not intervention
effects or a clinically validated prediction model.

## Repository contents

- `code/`: existing variable inventory, data preparation, scoring, core Cox,
  sensitivity, categorical, and historical figure/table scripts.
- `additional/`: existing October 2026 supplementary-analysis scripts,
  including prior-wave exposure, poststroke duration, last-interval CHARLS,
  activity-selection weighting, cumulative incidence, and figure generation.
- `docs/REPRODUCTION.md`: input files, dependencies, and execution boundaries.
- `docs/ANALYSIS_COVERAGE.md`: map of analyses to the available code.
- `docs/MEASUREMENT_AND_TIMING.md`: score, missingness, state, and time definitions.
- `docs/source_manifest.json`: SHA-256 hashes of the unmodified source scripts.

The scripts are archived as found. Their original statistical definitions and
historical filenames have not been silently revised during publication. Historical
plot labels and figure numbering can differ from the final manuscript.

## Data access and privacy

No participant-level data are distributed. This includes original Stata files,
derived RDS files, individual-level exports, fitted objects containing model
frames, and the local reproduction ZIP. Obtain the datasets through their
respective access procedures and comply with the applicable data-use terms:

- HRS: <https://hrs.isr.umich.edu/data-products>
- RAND HRS: <https://www.rand.org/well-being/social-and-behavioral-policy/centers/aging/dataprod/hrs-data.html>
- CHARLS: <https://charls.pku.edu.cn/>
- Gateway to Global Aging Data: <https://g2aging.org/>

Data access through these services is separate from access to this code.

## Environment

The source scripts use R and several CRAN packages. The analyses were developed
in Ubuntu; an existing analysis session used R 4.3.3. Consult the retained local
session records for the package versions used for a particular result.

Core dependencies include `haven`, `readr`, `dplyr`, `purrr`, `tibble`, `tidyr`,
and `survival`. Additional analyses and figures use `ggplot2`, `patchwork`,
`splines`, and `grid`; the latter two are supplied with R. Some inventory scripts
have their own package checks. The optional historical Word-table builder uses
Python and `python-docx`.

No R analyses were rerun to create this repository. This publication step does
not constitute independent numerical validation of every archived script.

## Reproduction entry points

The core model script is `code/09_primary_transition_models.R`. It reads cleaned
analytic RDS files from `data/derived/` relative to the working directory. Read
`docs/REPRODUCTION.md` before running anything. Preparation and BMI-cleaning
scripts can overwrite local derived files; use a separate working copy.

Do not run every historical script indiscriminately. Additional scripts have
ordered dependencies, and the stable CHARLS weighting script needs a locally
generated intermediate target dataset. Read the execution boundaries before
selecting the modules to run.

## Citation and licensing

Code is provided under the MIT License. The license applies to the code in this
repository, not to the underlying cohort data, third-party packages, or survey
documentation. Author metadata are provided in `CITATION.cff` and `.zenodo.json`.
Use the version-specific software citation when citing this archive:

Chen X, Wang J, Liu J, Cheng A. Adapted hBCS and stroke and mortality
transitions: research code. Version 1.0.0. Zenodo; 2026.
doi:10.5281/zenodo.23602467.
