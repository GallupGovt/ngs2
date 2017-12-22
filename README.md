# DARPA - NGS2 WITNESS

## Introduction
This repository contains the main code for analyzing data during the Next Generation Social Science (NGS2) program conducted by DARPA.

## R scripts
R is primary analysis tool used on WITNESS. This repository contains all the analytic scripts used to prepare and analyze data coming from the WITNESS team's data collection efforts.

### Empanelment
WITNESS empanels members of Gallup's nationally-representative panel. These data are collected via an external program and are inputs into the `NGS2_WITNESS_empanelment_prep.R` script. The input data are CSV file(s) and the output is a cleaned CSV file.

### Experiments
#### Prep
The `NGS2_WITNESS_Cycle1_experiment1_prep.R` and `NGS2_WITNESS_Cycle1_experiment2_prep.R` scripts prepare the data (CSV files) from the experimental platform for analysis. The output -- given many inputs CSVs -- are two CSV datasets. In addition, the `NGS2_WITNESS_Cycle1_difi_prep_exp2.R` prepares an additional set of analysis files.

#### Analysis
The following R scripts are all used for analysis:
* `NGS2_WITNESS_Cycle1_exploratory_exp1.R`
* `NGS2_WITNESS_Cycle1_exploratory_exp2.R`
* `NGS2_WITNESS_Cycle1_confirmatory_exp1.R`
* `NGS2_WITNESS_Cycle1_confirmatory_exp2.R`
* `NGS2_WITNESS_Cycle1_bayesian_exp1.R`
* `NGS2_WITNESS_Cycle1_bayesian_exp2.R`
* `NGS2_WITNESS_Cycle1_difi_exp2.R`

#### Presentation
There are two R markdown notebooks that are used for presentation of results: `NGS2_WITNESS_Cycle1_Exp1_Notebook.Rmd` and `NGS2_WITNESS_Cycle1_Exp2_Notebook.Rmd`.

#### Master script
The `NGS2_WITNESS_Cycle1_MASTER.R` will run the entire pipeline of scripts to produce output.

## Python scripts
In addition to the R scripts for analysis, there is a Python file to aid in constructing payment ledgers to know how much participants have earned through their experiments. This file -- `payout_calculations.py` calculates the monies earned by participants for a given cycle of experimentation. It requires a configuration file that contains local directory paths and other information.
