# Measurement and follow-up definitions

## Adapted score

The archived nine-domain score combines smoking, alcohol, physical activity,
BMI, hypertension, diabetes, sleep, mood, and social connection. It is a
survey-adapted research exposure, not the original clinical 21-point BCS.

The historical CHARLS alcohol algorithm derives its categories from frequency
fields. A field previously treated as drinking amount was found to be a
collapsed frequency encoding. It therefore does not establish measured alcohol
volume. The archived code preserves the score used for the existing results;
publication of the source is not a correction or validation of that measurement.
Revising this algorithm requires separately versioned scores and corresponding
analyses, not a silent replacement of the archived result.

BMI values below 12 or above 60 kg/m2 were set to missing in the cleaning step.
The `raw`, `scored`, and `analytic` filename suffixes refer to different stages
of derived data. Even a `person_wave_raw.rds` file is already a constructed
longitudinal dataset rather than the original downloaded Stata file.

## Risk sets and timing

The cause-specific models separate first recorded stroke, death without
previously recorded stroke, and poststroke death. CHARLS poststroke mortality
was not modeled. A death without a prior recorded stroke does not prove that
no stroke occurred between interviews.

The historical Cox scripts reset time at each interval, use nominal survey
year differences, assign events at interval ends, stratify by wave, and use
participant-clustered robust standard errors. Person-years from these intervals
are nominal observation years, not exact onset-based follow-up time.

Schoenfeld ERROR/NA output must not be interpreted as a passed proportional
hazards test. A wave interaction or grouped-time sensitivity model is not a
substitute for a successful Schoenfeld test.

CHARLS reversing reports are not automatically recoded as valid forward
transitions. The archived source retains the original state-handling rules.

## Missingness and supplementary analyses

The large CHARLS activity missingness proportion concerns eligible intervals,
not all source participants. Complete-score analytic data cannot reconstruct
the excluded records. Additional missingness work needs the scored interval
data before complete-case filtering.

Activity-selection weights concern intervals already complete on the other
eight domains and model covariates. They are distinct from survey weights and
from full complete-score selection weighting. Multiple imputation
was not performed by the archived supplementary scripts.

Prior-wave scores are joined by the exact previous wave; lagging a filtered
complete-case dataset could skip waves. Poststroke duration uses first recorded
stroke or a stated midpoint approximation, not an observed exact onset date.
The cumulative incidence analyses use a fixed entry score and are unadjusted;
their risk differences are not intervention effects.
