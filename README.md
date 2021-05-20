# auditData
auditData is a project focused on analyzing ties between tax audit rates, race and income across US counties.  A Shiny app has been developed to visualize asociations.  The tool created as a part of this research is geared toward informing experts in anti-racism and equity non-profits, think tanks and policy positions across the US.  While the datasets used in this project are largely quantitative in nature, the dashboard produced as a part of this research serves as a free and open-source tool to learn more about racial bias in the US tax system for those who don’t necessarily have technical backgrounds. 

## Installation
Prior to running the tax_ananlysis.Rmd or app.r files, be sure to install the folling packages:

```bash
install.packages("here")
install.packages("tidyverse")
install.packages("ISLR")
install.packages("tigris")
install.packages("leaflet")
```

## Usage
Bfeore running app.r, ensure that all objects from tax_analysis.Rmd appear in the environment, by running tax_analysis.Rmd.  

## Contributing
Pull requests are welcome.  For major changes, please open an issue first to discuss what you would like to change.

Note that data is originally sourced from [ProPublica](https://github.com/propublica/auditData) and the [US Census Bureau](https://www.census.gov/programs-surveys/acs).  
