This folder contains the master versions of the raw data for SCBI annual mortality census. The cleaned files to use in analysis are in the data folder.


## Raw data prior to 2021
Mortality_Survey_2014.csv to Mortality_Survey_2020.csv are raw data files as collected "manually" in the field. There was no continuous integration (CI) implemented then. These files are cleaned up by a separate (data cleaning script)[https://github.com/SCBI-ForestGEO/SCBImortality/blob/main/R_scripts/1_Clean_raw_data_and_unify_all_years.R] which saves the clean data in the "data" folder.

## Raw data in 2021 and 2022
In 2021, Fast Field Forms (FFF) and CI were implemented.
During the mortality census, all FFF (one per quadrat) are downloaded into one FFF excel file and pushed in the FFF_excel folder.
Every times this happens, the CI is triggered and reports are generated to inform the technicians of errors that need to be fixed in the field and errors/warnings that can be automatically fixed.

When there is no errors needing manual fixes, the  Mortality_Survey_[CURRENT_YEAR].csv is saved. The (data cleaning script)[https://github.com/SCBI-ForestGEO/SCBImortality/blob/main/R_scripts/1_Clean_raw_data_and_unify_all_years.R] will fix remaining errors and save the file in the "clean" data folder.

## Raw data in 2023
Mortality censused of that year happened with the 4th main census and was collected via an app and QA/QC checked in this repository: https://github.com/SCBI-ForestGEO/2023census
This (script)[https://github.com/SCBI-ForestGEO/2023census/blob/main/R_scripts/CI_QAQC_reports.R] saves the raw file In this folder. when in an interactive session (not when run via GitHub Action)

## Manual_fixes.csv
This file is read by the (data cleaning script)[https://github.com/SCBI-ForestGEO/SCBImortality/blob/main/R_scripts/1_Clean_raw_data_and_unify_all_years.R]. It lists all the known issues and implements the best known fix to the mortality data. Note that Fixes that need to happen in the census data happen in the (forestGEO data repo)[https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data] (see this (script)[https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/blob/master/R_scripts/FIX_ISSUES_IN_MAIN_CENSUS.R])

# what to do after a new census

1. save the data in a CSV file here, with name as Mortality_Survey_YYYY.csv
2. add new rows to (this file)[https://github.com/SCBI-ForestGEO/SCBImortality/blob/main/raw_data/standardizing_colnames.csv] to standardize column names
3. Edit Manual_fixes.csv as needed

