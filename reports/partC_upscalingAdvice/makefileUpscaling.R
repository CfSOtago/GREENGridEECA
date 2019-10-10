# loads data & runs a report

# Load some packages
library(GREENGridEECA)

libs <- c("data.table", # data munching
          "here", # here. not there
          "skimr") # skimming data for fast descriptives

GREENGridEECA::loadLibraries(libs) # should install any that are missing

GREENGridEECA::setup() # set data paths etc

# check

# parameters ----

# Data ----

#> Census data ----


#> GREEN Grid half hourly total dwelling power data ----
hhTotalLoadF <- paste0(repoParams$GreenGridData, "/gridSpy/halfHour/extracts/halfHourImputedTotalDemand.csv.gz")
hhTotalLoadDT <- data.table::fread(hhTotalLoadF)

#> GREEN Grid household survey data ----
hhAtttributesF <- paste0(repoParams$GreenGridData,"/survey/ggHouseholdAttributesSafe.csv.gz") 
hhAtttributesDT <- data.table::fread(hhAtttributesF)
  
# > defn of peak ----
amPeakStart <- hms::as_hms("07:00:00")
amPeakEnd <- hms::as_hms("09:00:00")
pmPeakStart <- hms::as_hms("17:00:00") # see https://www.electrickiwi.co.nz/hour-of-power
pmPeakEnd <- hms::as_hms("21:00:00") # see https://www.electrickiwi.co.nz/hour-of-power

# Functions ----
setPeakPeriod <- function(dt){
  # assumes hms exists
  dt[, peakPeriod := NA]
  dt[, peakPeriod := ifelse(hms < amPeakStart, "Early morning", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= amPeakStart & hms < amPeakEnd, "Morning peak", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= amPeakEnd & hms < pmPeakStart, "Day time", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= pmPeakStart & hms < pmPeakEnd, "Evening peak", peakPeriod)]
  dt[, peakPeriod := ifelse(hms >= pmPeakEnd, "Late evening", peakPeriod)]
  dt[, peakPeriod := forcats::fct_relevel(peakPeriod, 
                                          "Early morning",
                                          "Morning peak",
                                          "Day time",
                                          "Evening peak",
                                          "Late evening")]
  return(dt)
}


makeReport <- function(f){
  # default = html
  rmarkdown::render(input = rmdFile,
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(repoParams$repoLoc,"/docs/partC_upscalingAdvice_v", version, ".html")
  )
}

# code ----

# > Make report ----
# >> yaml ----
version <- "0.5"
title <- paste0("NZ GREEN Grid Household Electricity Demand Data")
subtitle <- paste0("EECA Data Analysis (Part C) Upscaling Advice Report v", version)
authors <- "Ben Anderson"


# >> run report ----
rmdFile <- paste0(repoParams$repoLoc, "/reports/partC_upscalingAdvice/upscaling.Rmd")
makeReport(rmdFile)


