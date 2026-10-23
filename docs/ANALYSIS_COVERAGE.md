# Analysis coverage

This is a source-code inventory, not a statement that all analyses have been
rerun. Existing statistical scripts are retained without numerical changes.

| Analysis or output | Located source | Coverage note |
| --- | --- | --- |
| Variable inventory and cohort preparation | `code/01` through `code/08` | Source Stata files are required locally. |
| BMI plausibility restriction | `code/12_clean_bmi_and_refresh_scores.R` | Changes derived RDS files in place; use a working copy. |
| Primary transition-specific Cox models | `code/09_primary_transition_models.R` | Uses wave strata and participant clustering. |
| Survey weighting, available-domain score, no-activity score | `code/10_sensitivity_models.R` | These weights are not the later selection weights. |
| Baseline descriptions and score categories | `code/11_descriptive_and_category_models.R` | Historical table numbering is retained. |
| Restricted cubic spline models and historical main figures | `code/15_redraw_main_figures.R` | Fits the spline models; figure numbering predates later revisions. |
| Prior-wave exposure and HRS poststroke duration | `additional/01_additional_analyses.R` | Exact wave matching; recorded stroke time is not exact onset. |
| Stable CHARLS lagged and final-interval models | `additional/02_stable_charls_and_ipw.R` | Uses joint wave/education strata for these supplementary models. |
| Activity-selection weighting and participant bootstrap | `additional/01` and `additional/02` | Restricted other-eight-domain-complete target; not full complete-score selection weighting. |
| Included/excluded participant descriptions | `additional/01_additional_analyses.R` | First eligible interval; not the later-entry baseline cohort. |
| Cumulative incidence, risk differences, and entry-score distribution | `additional/01_additional_analyses.R` | Fixed entry score, nominal event timing, first-gap censoring. |
| Updated flow counts and education categories | `additional/03_flow_and_education.R` | Reads existing derived cohort files. |
| October supplementary figure rendering | `additional/04_redraw_figures.R` | Needs locally generated results and historical aggregate inputs. |
| Historical age-spline, multiplicity, discrete-time, and wave-interaction analyses | Separate source scripts not yet located | Existing local results do not substitute for the missing source scripts. |
| Final per-SD presentation and final Word assembly | Not a declared end-to-end target | Historical scripts can use a different SD denominator; use the per-3-point contrast when comparing core coefficients. |

Death-reclassification, short death-interval, and full CHARLS complete-score
selection-weighting analyses are outside this release. No unrelated SEER cancer
analysis is included. Historical analyses whose standalone scripts were not
located are identified above; their local result files are not substituted for
source code. Do not reverse-engineer a statistical result from a rounded table value.
