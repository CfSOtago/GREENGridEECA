# GREEN Grid EECA
A repo supporting analysis of NZ GREEN Grid electricity demand data for EECA.

## Data

 * [New Zealand GREEN Grid Household Electricity Demand Study 2014-2018](http://reshare.ukdataservice.ac.uk/853334/)

## Reporting

### Part A – Data processing

 * Final report: ([v1.0](partA_dataProcessingReport_v1.0.html))

### Part B - Data analysis

 * Final report: ([v1.0])

### Part C – Provision of advice for upscaling 

 * Final report: ([v1.0])

## Code

Get it from [github](https://github.com/CfSOtago/GREENGridEECA).

This repo is structured as an R package. This means you (should) be able to install it using:

 * `devtools::install_github("CfSOtago/GREENGridEECA")`

If you try to do so and it fails, please check you have all the R packages that it [depends on](https://github.com/CfSOtago/GREENGridEECA/blob/master/DESCRIPTION). This includes the [GREENGridData](https://github.com/CfSOtago/GREENGridData) package (to save wheel re-invention) and various other goodies.

Note that to run the code you will need to [register with the UK Data Service](https://beta.ukdataservice.ac.uk/myaccount/credentials), agree to the re-use conditions and download the data.