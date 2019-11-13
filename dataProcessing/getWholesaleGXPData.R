# gets wholesale EA elec GXP data
# extracts data for 2 regions we want
# runs a report at the end

# Load some packages
library(GREENGridEECA)

libs <- c("curl", # pulling data off t'interweb
          "data.table", # data munching
          "drake", # data load pipeline
          "forcats", # for cats, obviously
          "here", # here. not there
          "skimr") # skimming data for fast descriptives

GREENGridEECA::loadLibraries(libs) # should install any that are missing

GREENGridEECA::setup() # set data paths etc

# check
message("GXP data: ", repoParams$gxpData, " (exists = ", file.exists(repoParams$gxpData), ")")

# parameters ----

# Functions ----

getGXPFileList <- function(dPath){
  # check for EA GXP files
  message("Checking for data files")
  all.files <- list.files(path = dPath, pattern = ".csv")
  dt <- as.data.table(all.files)
  dt[, fullPath := paste0(dPath, all.files)]
  message("Found ", nrow(dt))
  return(dt)
}


makeReport <- function(f){
  # default = html
  rmarkdown::render(input = rmdFile,
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(repoParams$repoLoc,"/docs/gxpReport_v", version, ".html")
  )
}

# code ----

# this is where we use drake if we can
drakePlan <- drake::drake_plan(
  gxpData = GREENGridEECA::loadGXPData(gxpFiles)
)

# > get data ----
years <- c("2013","2015")
forceRefresh <- "No" # force refresh even if we have data already
gxpFiles <- getGXPFileList(repoParams$gxpData) # will be empty if never run before so

if(nrow(gxpFiles) == 0){
  message("No data, refreshing!")
  # this just brute force overwrites whatever years you set above. Nothing fancy or clever
  GREENGridEECA::refreshGXPData(years) # generates melt warnings
  try(file.remove(temp.csv)) # curl side effect
  gxpFiles <- getGXPFileList(repoParams$gxpData) # should be some now
  message("Using drake to pre-load")
  make(drakePlan) # load them using drake
} else {
  message("Yep, we've got (some) data")
  if(forceRefresh == "Yes"){ # force refresh anyway
    message("Data refresh forced in any case - please wait")
    GREENGridEECA::refreshGXPData(years) # generates melt warnings
    try(file.remove(temp.csv)) # curl side effect
    gxpFiles <- getGXPFileList(repoParams$gxpData) # refresh file list
  } 
  message("Using drake to pre-load")
  make(drakePlan) # load them using drake
}

# > data stuff ----
gxpDataDT <- drake::readd(gxpData) # get gxp data back from wherever drake put it
# check data load worked
message("N rows GXP data: ", nrow(gxpDataDT), " from ", 
        min(gxpDataDT$rDate), " to ", max(gxpDataDT$rDate))
names(gxpDataDT)

# > Make report ----
# >> yaml ----
version <- "0.5"
title <- paste0("NZ GREEN Grid Household Electricity Demand Data")
subtitle <- paste0("EECA Data Analysis (Part B) GXP Data Report v", version)
authors <- "Ben Anderson"


# >> run report ----
rmdFile <- paste0(repoParams$repoLoc, "/dataProcessing/gxpReport.Rmd")

makeReport(rmdFile)


