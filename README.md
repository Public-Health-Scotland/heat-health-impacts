# heat-health-impacts


## Summary
This repository contains the code used by the PHS Climate Analyst team to run a DLNM (Distributed Lag Non-linear Model), an advanced statistical model that enables us to:
1)	Calculate the temperature thresholds at which the relative risk of death increases significantly across Scotland.
2)	Estimate the numbers and rates of deaths attributable to temperatures above these thresholds.

Also included is the calculation and analysis of excess deaths during heat episodes.


## Layout
Each folder is explained below:

- **data_linkage**:
Contains scripts for linking climate data (e.g. regional temperatures) from the Environmental Public Health Surveillance System (EPHSS), population and deprivation data with Scottish deaths data.

- **dlnm**:
Contains the code used in the DLNM, across various scripts.

- **excess_deaths**:
Contains the code for calculating and reporting excess deaths during heat episodes.

- **functions**:
Contains various functions used for wrangling data and summarising and displaying results.

- **setup**:
Contains the script for setting up the RStudio environment prior to data linkage. This is sourced as required.


