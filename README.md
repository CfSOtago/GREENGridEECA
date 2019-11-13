# GREENGridEECA
A repo supporting analysis of NZ GREEN Grid electricity demand data for EECA.

## Data

 * [New Zealand GREEN Grid Household Electricity Demand Study 2014-2018](http://reshare.ukdataservice.ac.uk/853334/);
 * [New Zealand Grid Export data](https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Grid_export) for 2015;
 * [New Zealand Census 2013 household counts](http://nzdotstat.stats.govt.nz/wbos/Index.aspx);

## Reporting

Best viewed at https://cfsotago.github.io/GREENGridEECA/

## Code

This repo is structured as an R package. This means you (should) be able to install it using:

 * `devtools::install_github("CfSOtago/GREENGridEECA")`

If you try to do so and it fails, please check you have all the R packages that it [depends on](https://github.com/CfSOtago/GREENGridEECA/blob/master/DESCRIPTION). This includes the [GREENGridData](https://github.com/CfSOtago/GREENGridData) package (to save wheel re-invention).

Note that to run the code you will need to [register with the UK Data Service](https://beta.ukdataservice.ac.uk/myaccount/credentials), agree to the re-use conditions and download the data.

### Package notes

 * Run `setup()` at the start of a .R or .Rmd script as it sources `./repoParams.R` which sets a range of parameters, including data file paths. 
 * Generally use a makefile.R to run `setup()`, set other parameters and load or pre-process data. Then call `render()` to generate your output from a .Rmd file that uses the pre-loaded data. This means you only need to re-run the report not the whole data loading process each time you change the .Rmd. You will find various examples of this approach in the [reports](/reports/) code. If you are feeling adventerous we recommend using [drake](https://ropenscilabs.github.io/drake-manual/) to further streamline your analysis.
 * `GREENGridEEECA` package functions should be called using the `GREENGridEEECA::function()` form so future users can identify when a function from this package is being used. Code for these functions is in /R.
 * Functions from other packages should be called in the same way for the same reason e.g. `lubridate::date()`.
 * Functions that are only defined in the .R or .Rmd file they are used in should just be called as `functionName()`. They should also be listed at the top of the file so they are easy to find!
