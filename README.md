# GREENGridEECA
A repo supporting analysis of NZ GREEN Grid electricity demand data for EECA.

## Data

 * [New Zealand GREEN Grid Household Electricity Demand Study 2014-2018](http://reshare.ukdataservice.ac.uk/853334/)

## Reporting

Best viewed at https://cfsotago.github.io/GREENGridEECA/

## Code

Get it from [github](https://github.com/CfSOtago/GREENGridEECA).

This repo is structured as an R package. This means you (should) be able to install it using:

 * `devtools::install_github("CfSOtago/GREENGridEECA")`

If you try to do so and it fails, please check you have all the R packages that it [depends on](https://github.com/CfSOtago/GREENGridEECA/blob/master/DESCRIPTION). This includes the [GREENGridData](https://github.com/CfSOtago/GREENGridData) package (to save wheel re-invention).

Note that to run the code you will need to [register with the UK Data Service](https://beta.ukdataservice.ac.uk/myaccount/credentials), agree to the re-use conditions and download the data.

### Package notes

 * Run `setup()` at the start of a .R or .Rmd script as it sources `./repoParams.R` which sets a range of parameters, including data file paths. 
 * Generally use a makefile.R to run `setup()`, set other parameters and load or pre-process data. Then call `render()` to generate your output from a .Rmd file that uses the pre-loaded data. This means you only need to re-run the report not the whole data loading process each time you change the .Rmd. You will find various examples of this approach in the [reports](/reports/) code. If  you are feeling adventerous we recommend using [drake](https://ropenscilabs.github.io/drake-manual/) to further streamline your analysis.
