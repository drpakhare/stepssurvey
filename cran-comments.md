## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local macOS (aarch64-apple-darwin), R 4.5.1
* GitHub Actions: ubuntu-latest (R release)
* R-hub2: ubuntu, fedora, windows (R-devel)
* win-builder: R-devel

## Downstream dependencies

This is a new package submission. There are no downstream dependencies.

## Resubmission

This is a resubmission. Changes made in response to reviewer feedback:

* Single-quoted all software names in DESCRIPTION ('Excel', 'Stata', 'SPSS', 'Word', 'HTML', 'CSV')
* Added method reference: Riley et al. (2016) <doi:10.2105/AJPH.2015.302962>
* Replaced \dontrun{} with \donttest{} for all executable examples; kept \dontrun{} only for examples requiring external data files or launching an interactive app
* Changed default output_dir from "outputs" to tempdir() in all file-writing functions
* Ensured options() are restored via on.exit() in setup_survey_design()
* Set LazyData: false (no data/ directory)
* Updated vignette examples to use tempdir()

## Notes

The package provides a complete analysis pipeline for WHO STEPS
(STEPwise Approach to NCD Risk Factor Surveillance) survey data. It has
been validated against published WHO fact sheets from nine countries
across six WHO regions (Moldova 2021, Mongolia 2019, Georgia 2016,
Afghanistan 2018, Algeria 2016, Ukraine 2019, Ecuador 2018,
Cabo Verde 2020, Bahamas 2019): 123 of 128 indicators (96 %) match
within 1 percentage point, and all 128 (100 %) have overlapping 95 %
confidence intervals.
