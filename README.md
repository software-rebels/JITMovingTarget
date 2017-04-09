# Replication Package

This repository contains the replication package for "Are Fix-Inducing Changes a Moving Target? A Longitudinal Case Study of Just-In-Time Defect Prediction". The package contains the JIT data and scripts that are needed to reproduce the Figures 5–8 from the paper.

# Required tools and packages

The following tools were installed on the machine where the scripts were originally executed:

  * R version 3.2.3

In addition, the following R packages were installed:

  * Formula 1.2-1
  * Hmisc 3.17-2
  * ggplot2 2.0.0
  * lattice 0.20-33
  * rms 4.4-2
  * SparseM 1.7
  * scales 0.3.0
  * survival 2.38-3
  * zoo 1.7-12

# Configuring the Experimental Setting

The experiment has several configurable parameters. These can be changed by editing the **scripts/experimental_setting.r** file. The file contains the following options:

Option | Description
------------ | -------------
FILE_THRESH  | Commits that touch more files than this value will be filtered out.
CHURN_THRESH | Commits that touch more lines than this value will be filtered out.
PROJECT_NAME | The name of the project that should be loaded from the **data** folder.
STRATA_PER_YEAR | A parameter to control the number of strata that should be created in the JIT data per year. Value of 2 (six months) and 4 (three months) are shown in the paper.
METRICS_FAMILIES | A list of families of metrics that are included in the fits of the JIT models.
FITTOOL | The function that is used to fit the models.
FITTOOL_PARMS | The parameters to be included with the FITTOOL function when it is called.
SPL | The function that should be used to fit splines in non-linear fits. Set to a NOOP function (included in the template file) if you want to (a) turn off nonlinear fits; or (b) try another FITTOOL that does not support splining.

# Executing the Package

In order to run the package, execute:

```bash
Rscript scripts/replicate.r
```
