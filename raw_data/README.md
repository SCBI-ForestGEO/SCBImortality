This folder contains the master versions of the raw data for SCBI annual mortality census. The cleaned files to use in analysis are in the data folder.


# Raw data prior to 2021
Mortality_Survey_2014.csv to Mortality_Survey_2020.csv are raw data files as collected "manually" in the field. There was no continuous integration (CI) implemented then. These files are cleaned up by a separate script [insert link] which saves the clean data in the "data" folder.

# Raw data starting 2021
In 2021, Fast Field Forms (FFF) and CI were implemented.
During the mortality census, all FFF (one per quadrat) are downloaded into one FFF excel file and pushed in the FFF_excel folder.
Every times this happens, the CI is triggered and reports are generated to inform the technicians of errors that need to be fixed in the field and errors/warnings that can be automatically fixed.

When there is no errors needing manual fixes, the  Mortality_Survey_[CURRENT_YEAR].csv is saved. This script [insert link] will fix remaining errors and save the file in the "clean" data folder.
