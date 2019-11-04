# loads data & runs a report

# Load some packages
library(GREENGridEECA)

libs <- c("data.table", # data munching
          "drake", # data pre-loading etc
          "here", # here. not there
          "skimr") # skimming data for fast descriptives

GREENGridEECA::loadLibraries(libs) # should install any that are missing

GREENGridEECA::setup() # set data paths etc

# parameters ----

# > defn of peak ----
amPeakStart <- hms::as_hms("07:00:00")
amPeakEnd <- hms::as_hms("09:00:00")
pmPeakStart <- hms::as_hms("17:00:00") # see https://www.electrickiwi.co.nz/hour-of-power
pmPeakEnd <- hms::as_hms("21:00:00") # see https://www.electrickiwi.co.nz/hour-of-power

# funtions ----

getGXPFileList <- function(dPath){
  # check for EA GXP files
  message("Checking for data files")
  all.files <- list.files(path = dPath, pattern = ".csv") # will pick up .csv & .csv.gz
  dt <- as.data.table(all.files)
  dt[, fullPath := paste0(dPath, all.files)]
  message("Found ", nrow(dt))
  return(dt)
} # should be in package functions

getGXPData <- function(files){
  if(nrow(files) == 0){
    message("No data!")
  } else {
    message("Yep, we've got (some) data")
    dt <- GREENGridEECA::loadGXPData(gxpFiles)
    # remove the date NAs here (DST breaks)
    dt <- dt[!is.na(date)]
  }
  return(dt)
}

getGgHalfHourLoad <- function(f){
  dt <- data.table::fread(hhTotalLoadF) # do in drake
  return(dt)
}

# Code ----
# Load all data here, not in .Rmd

#> Census data ----
# will load the latest version
ipfCensusF <- paste0(repoParams$censusData, "/data/processed/2013IpfInput.csv")
ipfCensusDT <- data.table::fread(ipfCensusF)

#> GREEN Grid half hourly total dwelling power data ----
hhTotalLoadF <- paste0(repoParams$GreenGridData, 
                       "/gridSpy/halfHour/extracts/halfHourImputedTotalDemand.csv.gz")
# hhTotalLoadDT <- data.table::fread(hhTotalLoadF) # do in drake

#> GREEN Grid half hourly heat pump data ----
hhHPLoadF <- paste0(repoParams$GreenGridData, 
                    "/gridSpy/halfHour/extracts/halfHourHeatPump.csv.gz")
# load in drake


#> HCS 2015 heat source
hcs2015DT <- data.table::fread(paste0(here::here(), 
                                      "/data/input/hcs2015HeatSources.csv"))

#> GREEN Grid household survey data ----
hhAttributesF <- paste0(repoParams$GreenGridData,
                        "/survey/ggHouseholdAttributesSafe_2019-10-20.csv.gz") # latest version 
hhAttributesDT <- data.table::fread(hhAttributesF)
# fix PV inverter
hhAttributesDT[, `PV Inverter` := ifelse(`PV Inverter` == "", "No", "Yes")]


# will load the latest version
ipfSurveyDT <- data.table::fread(paste0(repoParams$GreenGridData, 
                                        "/survey/ggIpfInput.csv"))

#> ipf weights data from previous run of model
ipfWeightsF <- paste0(repoParams$GreenGridData, 
                      "ipf/nonZeroWeightsAu2013.csv")
ipfWeightsDT <- data.table::fread(ipfWeightsF)

years <- c("2013","2015")

gxpFiles <- getGXPFileList(repoParams$gxpData) # will be empty if never run before so

# drake plan ----
# this is where we use drake if we can
plan <- drake::drake_plan(
  gxpData = getGXPData(gxpFiles),
  ggHHData = getGgHalfHourLoad(hhTotalLoadF),
  ggHPData = data.table::fread(hhHPLoadF) 
)


plan # test the plan
make(plan) # run the plan, re-loading data if needed

makeReport <- function(inF,outF){
  # default = html
  rmarkdown::render(input = inF,
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = outF
                    )
}

# code ----

# > Make report ----
# >> yaml ----
version <- "0.8"
title <- paste0("NZ GREEN Grid Household Electricity Demand Data")
subtitle <- paste0("EECA Data Analysis (Part C) Upscaling Advice Report v", version)
authors <- "Ben Anderson"

# quick test - up to date?
file.info(ipfWeightsF)
file.info(ipfCensusF)
file.info(hhAttributesF)

# >> run report ----
inF <- paste0(repoParams$repoLoc, "/reports/partC_upscalingAdvice/upscaling.Rmd")
outF <- paste0(repoParams$repoLoc,"/docs/partC_upscalingAdvice_v", version, ".html")
makeReport(inF,outF)


