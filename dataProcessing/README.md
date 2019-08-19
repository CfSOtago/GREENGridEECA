# GREENGridEECA
A repo supporting analysis of NZ GREEN Grid electricity demand data for EECA.

## Data

 * [New Zealand GREEN Grid Household Electricity Demand Study 2014-2018](http://reshare.ukdataservice.ac.uk/853334/)

## Code

This repo is structured as an R package. This means you (should) be able to install it using:

 * `devtools::install_github("CfSOtago/GREENGridEECA")`
 
Note that to run the code you will need to [register with the UK Data Service](https://beta.ukdataservice.ac.uk/myaccount/credentials), agree to the re-use conditions and download the data.

### makeHalfHourlyData.R

This script takes all of the clean 1 minute observations for all households and aggregates them by circuit to half-hourly observations by taking the:

 * mean
 * min
 * max
 * sd
 * N observations
 
 for powerW across each half-hour within each circuit for each household. This includes the [imputed total load](https://cfsotago.github.io/GREENGridData/reportTotalPower_circuitsToSum_v1.1.html) which is also aggregated in the same way _for all households_.
 
 The results are saved as 1 file per household each of which will have the following structure:
 <pre>
    linkID                            circuit   r_dateTimeHalfHour meanPowerW nObs sdPowerW minPowerW maxPowerW
rf_47 Heat Pump & 2 x Bathroom Heat$4171 2015-03-25T02:00:00Z      0.000   16   0.0000         0      0.00
rf_47 Heat Pump & 2 x Bathroom Heat$4171 2015-03-25T02:30:00Z    175.006   30 432.3806         0   1916.25
rf_47 Heat Pump & 2 x Bathroom Heat$4171 2015-03-25T03:00:00Z      0.000   30   0.0000         0      0.00
rf_47 Heat Pump & 2 x Bathroom Heat$4171 2015-03-25T03:30:00Z      0.000   30   0.0000         0      0.00
rf_47 Heat Pump & 2 x Bathroom Heat$4171 2015-03-25T04:00:00Z      0.000   30   0.0000         0      0.00
rf_47 Heat Pump & 2 x Bathroom Heat$4171 2015-03-25T04:30:00Z      0.000   30   0.0000         0      0.00
 </pre>
 
  * linkiD: anonyised ID to link to the [household data](http://reshare.ukdataservice.ac.uk/853334/);
  * circuit: the monitored circuit, includes imputed total load as a 'circuit'
  * r_dateTimeHalfHour: date/time in UTC ([what does the T & Z mean?](https://stackoverflow.com/questions/8405087/what-is-this-date-format-2011-08-12t201746-384z))
  * mean/nObs/sd/min/max power in watts
  

## Reporting

Best viewed at https://cfsotago.github.io/GREENGridEECA/
