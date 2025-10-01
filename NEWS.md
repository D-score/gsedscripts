# gsedscripts 0.26.0.9000

- Adapts scripts to gsed3 lexicon
- Moves `calculate_DIF_classification()` and `calculate_DIF_table()` from gsedscripts (0.27.0) to dfine package (0.13.0)
- Moves `build_database_fixed.R` from `gsedread` to `gsedscripts` package

# gsedscripts 0.26.0

- Add some improvements to scripts
- Prepare for reproducibility

# gsedscripts 0.25.0

- Adds reporting outline `Rmd/coremodel_aug25.Rmd` to document the reporting process for the GSED key
- Adds many scripts to `inst/scripts/phase2` directory to help fitting the new phase_1+2 models
- Removes `DUCKDB_LOCAL` system variable from all scripts
- All scripts depends now only on system variables `GSED_PHASE1` and `GSED_PHASE2`
- Adds `calculate_DIF_classification()` to test for DIF by phase and country
- Extend scripts `plot_pass_age.R` and `plot_pass_dscore.R` with BSID and apply system variables `GSED_PHASE1` and `GSED_PHASE2`
- Adds generic script `phase2/fit_core_model.R` which replaces `293_phase1_long.R`, `293_phase2_wide.R`
- Updates the following phase2 scripts:

  - descriptives/plot_pass_age
  - models/293_0_ph

# gsedscripts 0.23.0

- Adds starting phase2 scripts:

  - `plot_pass_age.R`
  - `293_phase1_long.R`
  - `293_phase2_wide.R`

- These scripts replace the private `dmetric` package by the public `dfine` package.

- The experimental `lean2` class is no longer supported, and removed.

# gsedscripts 0.22.0

- Adds script `inst/scripts/models-phase1/293_0_test_phase1`. This script refits the 293_0 model on phase 1 data using better duplicate removal, duckbd dataset, and removal of inter-rater scores.

# gsedscripts 0.21.0

- Adds script `inst/scripts/DIF_demo_residuals` 

# gsedscripts 0.20.0

- Adds two working scripts
  + `inst/scripts/models/818_6_enhancements.R`, a further development of `818_6.R`
  + `inst/scripts/293_0_BSID_experimental.R`, a script to explore a better linking method, using the BSID-III and the 293_0 model. Needs to be checked and - if OK - generalized 

# gsedscripts 0.19.0

- Extends `scripts/DIF_demo.R` with >2 groups

# gsedscripts 0.18.0

- Updates `scripts/models/293_0.R` to use DIF tabulation function and explore Yen's Q3 method for exploring dimensionality
- Updates `scripts/models/lf_155_0.R` to feature newer functions and to compact code
- Adds `scripts/DIF_demo_2groups.R` to shows how to use the `difR` package in the SMOCC data for testing 2-group DIF with MH and logistic regression methods
- Adds `calculate_DIF_table()` for testing DIF

# gsedscripts 0.17.0

* Substantial update 293_0 script
  - better stepwise approach
  - use of new functions to reduce copies
  - add item deletion of rare categories
  - add code to test D-score - logit alignment
  - check anchors
  - graph item and person fit
  - add tests for DIF by cohort

* Generalizes `make_wide()` to provide more records for analysis

# gsedscripts 0.16.0

* Updates function `update_required_packages()` to automate modelling scripts
* Adds some dependencies into Suggests

# gsedscripts 0.15.0

* Adds function `update_required_packages()` to automate modelling scripts
* Adds function `calculate_administrative()` for deriving standard administrative variables
* Adds function `make_wide()` to combine SF, LF and BSID data from the first observation period onto one record per child
* Adds a script `inst/scripts/compare_domain_taus_293.R` to inspect the overlap between three streams of the LF a as way to informally inspect the uni-dimensionality of the LF. Used for the July 2024 version of the phase1 paper.
* Adds a scripts `inst/scripts/extract_equate_table.R` to extract the equate table to compare keys `gsed1912` and `gsed2406`. Used for the July 2024 version of the phase1 paper.

# gsedscripts 0.14.0

* Adds `Rmd/20240802_logit.Rmd` to demonstrate that logit and D-score scales are consistent for LF and SF, but not for other instruments
* Adds new procedure `inst/script/models/by3_extension.R` to link BSID-III under fixing SF/LF tau estimates to those in model 293_0, leading to consistent logit and D-score scale for BSID-III

# gsedscripts 0.13.0

* Perform updates of model 293_0 using additional LF data for BGD

# gsedscripts 0.12.0

* Replaces `fit_phase1_healthy_reference.R` by `fit_preliminary_standards.R`

# gsedscripts 0.11.0

* Adds a new fuzzymatching method to `inst/scripts/counts.R` and `inst/scripts/293_0.R` that takes only the first LF, SF and BSID before matching. As a result, each row corresponds to a unique subjid (child). This does not use all data, but is much easier to explain and report. The impact of the method on the item difficulty estimates is negligible (r > 0.99).

# gsedscripts 0.10.0

* Adds script `inst/scripts/fit_phase1_healthy_references.R` to calculate new healthy references for the GSED key
* Copies code for creating Dutch references for the GSED key into file `inst/scripts/fit_phase1_dutch_references.R`

# gsedscripts 0.9.0

* Extends script `inst/scripts/fit_models.R` a refit using `dscore 1.8.8`
* Adds script `inst/scripts/compare_algorithm.R` to compare models "20221201_remodel/293_0" and "20240601/293_0" using `dscore 1.8.8`
* Adds script `inst/scripts/compare_models.R` to compare models "20221201_remodel/293_0" and "20240601/293_0" using the `dscore 1.8.8`
* Extends script `inst/scripts/fit_phase1_references.R` with calculation of the healthy subsample references from the GSED key
* Extends script `inst/scripts/fit_phase1_references.R` with calculation of the Dutch references from the GSED key

# gsedscripts 0.8.1

* Changes path in source scripts for GSED SF and BSID so that it will run from any directory, as long as the R packages dmetric and gsedscript are installed on the user's machine.

# gsedscripts 0.8.0

* Adds source scripts for document that explain scoring GSED LF and GSED SF

# gsedscripts 0.7.0

* Make scripts better useable as external call

# gsedscripts 0.6.0

* Updates modeling scripts
* Updates reports that documents actions taken to solve LF item order problem

# gsedscripts 0.5.0 

* Adds document to log actions taken to solve the LF item order problem
* Updates data reading scripts `assemble_data.R` and `edit_data.R`

# gsedscripts 0.4.0

* Adds inst/scripts/counts.R to produces basic counts on LF, SF and BSID

# gsedscripts 0.3.0

* Adds key update and scoring Rmd's for reporting
* Adds scripts/export_gsed_sample.R which draws 10 sample cases
* Moves .txt files to ../export
* Moves .pdf files to ../plots

# gsedscripts 0.2.0

* Added a `NEWS.md` file to track changes to the package.
