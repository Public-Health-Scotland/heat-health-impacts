# heat-health-impacts

## Summary
This repository contains the code used by the PHS Climate Analyst team to run a DLNM (distributed lag non-linear model), an advanced statistical model that enables us to:
1)	Calculate the temperature thresholds at which the relative risk of death or hospitalisation increases significantly across Scotland.
2)	Estimate the numbers and rates of deaths and hospitalisations attributable to temperatures above these thresholds.

## Layout
Each folder is explained below:

- **condition_analysis**:
Contains scripts for data linkage and analysis of heat-related hospitalisation causes in Scotland.

- **data_linkage**:
Contains scripts for linking climate data (e.g. regional temperatures) from the Office of National Statistics (ONS), population and deprivation data with both Scottish a) hospitalisations and b) deaths data.

- **dlnm**:
Contains the code used in the DLNM, across various scripts.

- **functions**:
Contains various functions used in summarising and displaying the DLNM results.

- **lookups**:
Contains lookup codes (e.g. for medical conditions) for reference by the data linkage scripts.

- **setup**:
Contains the script for setting up the RStudio environment prior to data linkage. This is sourced as required.

## Instructions for use

[insert content here]









