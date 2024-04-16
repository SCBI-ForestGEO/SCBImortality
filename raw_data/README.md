This folder contains the master versions of the raw data for SCBI annual mortality census. **The cleaned files to use in analysis are in the data folder**.


## Raw data prior to 2021
Mortality_Survey_2014.csv to Mortality_Survey_2020.csv are raw data files as collected "manually" in the field. There was no continuous integration (CI) implemented then. These files are cleaned up by a separate (data cleaning script)[https://github.com/SCBI-ForestGEO/SCBImortality/blob/main/R_scripts/1_Clean_raw_data_and_unify_all_years.R] which saves the clean data in the "data" folder.

## Raw data in 2021 and 2022
In 2021, Fast Field Forms (FFF) and CI were implemented.
During the mortality census, all FFF (one per quadrat) are downloaded into one FFF excel file and pushed in the FFF_excel folder.
Back then, everytime this happened, the CI was triggered and reports were generated to inform the technicians of errors that needed to be fixed in the field and errors/warnings that can be automatically fixed.

When there is no errors needing manual fixes, the  Mortality_Survey_[CURRENT_YEAR].csv is saved. The (data cleaning script)[https://github.com/SCBI-ForestGEO/SCBImortality/blob/main/R_scripts/1_Clean_raw_data_and_unify_all_years.R] will fix remaining errors and save the file in the "clean" data folder.

## Raw data in 2023
Mortality censused of that year happened with the 4th main census and was collected via an app and QA/QC checked in this repository: https://github.com/SCBI-ForestGEO/2023census
This (script)[https://github.com/SCBI-ForestGEO/2023census/blob/main/R_scripts/CI_QAQC_reports.R] saves the raw file In this folder. when in an interactive session (not when run via GitHub Action)

## Raw data in 2024
Starting 2024, data is collected through the Field Maps app (created by Jess Shue). After the data is collected in the field, and the tablet is connected to the wifi, the data on the Smithonsian's Maps ArcGIS (here)[https://si.maps.arcgis.com/home/item.html?id=c40b9816009f4fe985c9a0e65e7df885], and the field tech can download that data and push it to the Field_Maps folder. The data is split into two csv files, one for the main stem of the trees one for any secondary stems that these trees may have.
A GitHub Action gets triggered when these files are updated (at the end of each field day) and it generates reports  to inform the technicians of errors that need to be fixed in the field.

## Manual_fixes.csv
This file is read by the (data cleaning script)[https://github.com/SCBI-ForestGEO/SCBImortality/blob/main/R_scripts/1_Clean_raw_data_and_unify_all_years.R]. It lists all the known issues and implements the best known fix to the mortality data. Note that Fixes that need to happen in the census data happen in the (forestGEO data repo)[https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data] (see this (script)[https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/blob/master/R_scripts/FIX_ISSUES_IN_MAIN_CENSUS.R])

# what to do after a new census

1. save the data in a CSV file here, with name as Mortality_Survey_YYYY.csv
2. add new rows to (this file)[https://github.com/SCBI-ForestGEO/SCBImortality/blob/main/raw_data/standardizing_colnames.csv] to standardize column names
3. Edit Manual_fixes.csv as needed

