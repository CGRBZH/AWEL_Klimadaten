# AWEL - Interaktive Stadtklima Auswertungen



This project encourages reproducible work, such that anyone who opens the project folder should be able to repeat the analyses and replicate the results.

Therefore we use R projects to organise our work.To keep things tidy, we use a sub-directory structure resembling the following:

```
project/
  - README.Rmd # R project description
  - planning_management.Rmd # Project management structures and workflows
  - reporting.Rmd # Analyses, description & documentation
  - set-up.R  # Required packages
  - R/ # For R code
  - input # Data files
  - graphics/
  - output/ # Results
  
```

### Installation rOstluft & rOstluft.plot

The source code of rOstluft is hosted on GitHub - see [rOstluft Tutorial on GitHub](https://ostluft.github.io/rOstluft/articles/articles/tutorial.html)

Installation with the help of the devtools package is the most straightforward option:

```
#install.packages("devtools")
devtools::install_github("Ostluft/rOstluft")
devtools::install_github("Ostluft/rOstluft.plot")
```

Additionally, the package aws.s3 has to be installed manually from the cloudyr repository because the CRANR version is outdated:

```
install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
```

#### Proxy Error?

1. Execute `usethis::edit_r_profile()` in the R console
2. Modify the proxy settings to be 
    ```
    Sys.setenv(https_proxy="proxy.kt.ktzh.ch:8080")
    Sys.setenv(http_proxy="proxy.kt.ktzh.ch:8080")
    ```
3. Restart R for changes to take effect

### Prerequisites

All access data for the user Statistikamt_Kanton_Zuerich is held by Corinna Grobe.  

The easiest way to access the rOstluft AWS credentials (API keys and authentication tokens) is to create a `.Renvirion` file in the directory of the RStudio project or in the HOME directory of the user. The path to the directory corresponds to the output of `Sys.getenv("HOME")`.

Content of the `.Renvirion` file:

```
        AWS_ACCESS_KEY_ID = "XXXXXXXXXXXXXXXXXXXX"
        AWS_SECRET_ACCESS_KEY = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        AWS_DEFAULT_REGION = "eu-central-1" 
```

Run the [Initialise_rOstluft_DataStore.R](http://10.73.108.152:8788/STAT/AWEL_Interaktive_Stadtklima_Auswertungen/src/branch/master/R/Initialise_rOstluft_DataStore.R) script to verify the settings are correct and working.

### Required packages
All required packages and their version information are listed in the [set-up.R](http://10.73.108.152:8788/STAT/AWEL_Interaktive_Stadtklima_Auswertungen/src/branch/master/set-up.R) file and can be loaded directly from there.

```
        require(rOstluft) # Version ‘1.4.0’
        require(rOstluft.plot)
        require(readr) # Version ‘1.3.1’
        require(dplyr) # Verion ‘1.0.0’
        require(tidyr) # Version ‘1.1.0’
        require(lubridate) # Version ‘1.7.9’
        require(ggplot2) # Version ‘3.3.2’
        require(statR) # Version ‘0.0.0.9000’
```

## 4. Project documentation

- Outlining of project description, planning, and workflows is documented in [palnning_management.Rmd](http://10.73.108.152:8788/STAT/AWEL_Interaktive_Stadtklima_Auswertungen/src/branch/master/planning_management.Rmd)

- Reporting on analyses, descriptions, and additional documentation is done in [reporting.Rmd](http://10.73.108.152:8788/STAT/AWEL_Interaktive_Stadtklima_Auswertungen/src/branch/master/reporting.Rmd)

## 5. Other resources

Several resources proved invaluable when building this app, including:

- A [tutorial by Florianne Verkroost](https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/) on building interactive maps;
- The [COVID-19 tracker](https://vac-lshtm.shinyapps.io/ncov_tracker/) app and [associated code](https://github.com/eparker12/nCoV_tracker);
- The [RStudio Leaflet tutorials](https://rstudio.github.io/leaflet/);
- The [RStudio Shiny examples](https://github.com/rstudio/shiny-examples);
- The general [RStudio Shiny tutorial](https://shiny.rstudio.com/tutorial/);
- [Using Leaflet with Shiny](https://rstudio.github.io/leaflet/shiny.html).



