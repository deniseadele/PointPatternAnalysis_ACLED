# pointpatternACLED
##### Point Pattern Analysis: Case Study on Armed Conflict in Southern Asia
This repository stores the files for a project undertaken for the course ISSS608- Visual Analytics offered in SMU MITB. Please view the R shiny application [here](https://deniseadele-chs.shinyapps.io/pointpatternACLED/).


## Setup
As the raw datafiles are huge and cannot be uploaded to the repository, the files are read into R and saved as RDS objects in the **Data** folder.

If you'd like to load in your own dataset, please run the script in `PrepareDataFiles.R` to prepare the required RDS files that will be used in `app.R`.

The datafiles were downloaded from the sources listed below:
- [ACLED](https://www.acleddata.com/) : 
	-The armed conflict dataset was downloaded from ACLED (Armed Conflict Location and Event Data Project). For this project, the analysis was focused on Southern Asia during a 4 year period (Jan 2016 to Dec 2019)
- [GADM](https://gadm.org/data.html) : The geopackages that map the administrative areas of Southern Asia countries (Pakistan, Bangladesh, Sri Lanka, Nepal, India) were downloaded from the database of Global Administrative Areas (GADM)


## miscellaneous files
Besides `app.R` and `PrepareDataFiles.R`, the other R scripts were originally created to run our analysis within Rstudio environment. **They are not required to run the shiny app.**

## FAQ/ Troubleshooting
Please contact deniseadele92@gmail.com for more information


