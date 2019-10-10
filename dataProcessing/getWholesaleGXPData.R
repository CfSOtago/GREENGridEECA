# gets wholesale EA elec GXP data
# extracts data for 2 regions we want
# runs a report at the end

# Load some packages
library(GREENGridEECA)

libs <- c("curl", # pulling data off t'interweb
          "data.table", # data munching
          "forcats", # for cats, obviously
          "ggplot2", # for plots
          "here", # here. not there
          "skimr") # skimming data for fast descriptives

GREENGridEECA::loadLibraries(libs) # should install any that are missing

GREENGridEECA::setup() # set data paths etc

# check
message("GXP data: ", repoParams$gxpData, " (exists = ", file.exists(repoParams$gxpData), ")")


# parameters ----

# NZ Electricity Authority data ----

# > EA GXP data location ----
rDataLoc <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Grid_export/"

# > defn of peak ----
amPeakStart <- hms::as.hms("07:00:00")
amPeakEnd <- hms::as.hms("09:00:00")
pmPeakStart <- hms::as.hms("17:00:00") # see https://www.electrickiwi.co.nz/hour-of-power
pmPeakEnd <- hms::as.hms("21:00:00") # see https://www.electrickiwi.co.nz/hour-of-power

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

getData <- function(f){
  # f <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Grid_export/201907_Grid_export.csv"
  req <- curl::curl_fetch_disk(f, "temp.csv") # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
  if(req$status_code != 404){ #https://cran.r-project.org/web/packages/curl/vignettes/intro.html#exception_handling
    df <- readr::read_csv(req$content)
    print("File downloaded successfully")
    dt <- cleanGXP(df) # clean up to a dt
    return(dt)
  } else {
    print(paste0("File download failed (Error = ", req$status_code, ") - does it exist at that location?"))
  }
}

cleanGXP <- function(df){
  # takes a df, cleans & returns a dt
  dt <- data.table::as.data.table(df) # make dt
  # reshape the data as it comes in a rather unhelpful form
  reshapedDT <- melt(dt,
                     id.vars=c("POC","NWK_Code", "GENERATION_TYPE", "TRADER","TRADING_DATE",
                               "UNIT_MEASURE", "FLOW_DIRECTION","STATUS" ),
                     variable.name = "Time_Period", # converts TP1-48/49/50 <- beware of these ref DST!
                     value.name = "kWh" # energy 
  )
  reshapedDT <- setEAGenTimePeriod(reshapedDT) # set time periods to something intelligible as rTime
  reshapedDT <- reshapedDT[, rDate := lubridate::dmy(TRADING_DATE)] # fix the dates so R knows what they are. Would be nice if these matched the Gen data
  reshapedDT <- reshapedDT[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime
  return(reshapedDT)
}


setEAGenTimePeriod <- function(dt){
  # convert the given time periods (TP1 -> TP48, 49. 50) to hh:mm
  dt <- dt[, c("t","tp") := tstrsplit(Time_Period, "P")]
  dt <- dt[, mins := ifelse(as.numeric(tp)%%2 == 0, "30", "00")] # set to start point. 
  # TPX -> if X is even = 30 past the hour
  # So TP1 -> 00:00, TP2 -> 00:30, TP3 -> 01:00, TP4 -> 01:30 etc
  dt <- dt[, hours := floor((as.numeric(tp)+1)/2) - 1]
  dt <- dt[, strTime := paste0(hours, ":", mins, ":00")]
  dt <- dt[, rTime := hms::as.hms(strTime)]
  # head(dt)
  #dt <- dt[, c("t","tp","mins","hours","strTime") := NULL]  #remove these now we're happy
  return(dt)
}

refreshGXPData <- function(years){
  # assumes months is a list of months of the form "201801" to match EA url
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") # srsly
  for(y in years){
    for(m in months){
      rDataF <- paste0(rDataLoc, y, m, "_Grid_export.csv")
      print(paste0("Getting, processing and cleaning ", y,m, " (", rDataF, ")"))
      dt <- getData(rDataF)
      of <- paste0(repoParams$gxpData, "/EA_",y, m, "_GXP_MD.csv")
      data.table::fwrite(dt, file = of)
      GREENGridEECA::gzipIt(of)
    }
  }
}

testGxp <- function(dt){
  dt[, .(meankWh = mean(kWh),
         nObs = .N,
         nRegions = uniqueN(regionName),
         nPOCs = uniqueN(node),
         nNetworks = uniqueN(NWK_Code),
         nGenTypes = uniqueN(GENERATION_TYPE)), keyby = .(hours, Time_Period)]
  
}


getGXPFileList <- function(dPath){
  # check for EA GXP files
  message("Checking for data files")
  all.files <- list.files(path = dPath, pattern = ".csv")
  dt <- as.data.table(all.files)
  dt[, fullPath := paste0(dPath, all.files)]
  message("Found ", nrow(dt))
  return(dt)
}


loadGXPData <- function(files){
  # load the EA GXP data
  # https://stackoverflow.com/questions/21156271/fast-reading-and-combining-several-files-using-data-table-with-fread
  # this is where we need drake
  # and probably more memory
  message("Loading ", nrow(files), " GXP files")
  l <- lapply(files$fullPath, data.table::fread)
  dt <- rbindlist(l)
  setkey(dt, rDateTime)
  try(file.remove("temp.csv")) # side effect
  # fix dates & times ----
  dt <- dt[!is.na(rTime)] # drop the TP49 & TP50
  dt[, rDateTime := lubridate::as_datetime(rDateTime)]
  dt[, rDateTime := lubridate::force_tz(rDateTime, tzone = "Pacific/Auckland")]
  dt[, date := lubridate::date(rDateTime)]
  dt[, month := lubridate::month(rDateTime)]
  dt[, day_of_week := lubridate::wday(rDateTime, label = TRUE)]
  dt[, hms := hms::as.hms(rDateTime)] # set to middle of half-hour
  dt[, halfHour := hms::trunc_hms(hms, 30*60)] # truncate to previous half-hour
  
  # Create factor for weekdays/weekends ----
  dt[, weekdays := "Weekdays"]
  dt[, weekdays := ifelse(day_of_week == "Sat" |
                            day_of_week == "Sun", "Weekends", weekdays)]
  # locate in peak/not peak ----
  dt <- setPeakPeriod(dt)
  
  return(dt)
}

testData <- function(){
  
  table(gxpDataDT$POC) # which one(s) do we need? Vince (EECA) knows...
  
  # find the overall GXP peaks
  top100DT <- head(gxpDataDT[order(-gxpDataDT$kWh)], 100)
  
  plotDT <- top100DT[, .(nDates = .N), keyby = .(rDate, POC)]
  
  ggplot2::ggplot(plotDT, aes(x = rDate, y = POC, fill = nDates)) + geom_tile()
  # OK, so that's all Tiwai
  
  top100DT <- head(gxpDataDT[POC != "TWI2201"][order(-gxpDataDT$kWh)], 100)
  
  plotDT <- top100DT[, .(nDates = .N), keyby = .(rDate, POC)]
  
  ggplot2::ggplot(plotDT, aes(x = rDate, y = POC, fill = nDates)) + 
    geom_tile() +
    theme(axis.text.x = element_text(angle = 90))
  
  # Let's draw a map
  # when we've added lat/long
}

extractData <- function(){
  # load the POC lookup table Vince sent
  f <- paste0(here::here(), "/data/gxp-lookup.csv")
  gxpDT <- data.table::fread(f)
  
  head(gxpDT)
  head(gxpDataDT)
  
  setkey(gxpDT, node)
  setkey(gxpDataDT, POC)
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

# > get data ----
years <- c("2015")

gxpFiles <- getGXPFileList(repoParams$gxpData) # will be empty if never run before so

if(nrow(gxpFiles) == 0){
  message("No data, refreshing!")
  # this just brute force overwrites whatever years you set above. Nothing fancy or clever
  refreshGXPData(years) # use this line to force a refresh
  gxpFiles <- getGXPFileList(repoParams$gxpData) # should be some now
  gxpDataDT <- loadGXPData(gxpFiles)
} else {
  message("Yep, we've got (some) data")
  gxpDataDT <- loadGXPData(gxpFiles)
}

# > data stuff ----
testData()
extractData()

head(taranakiDT)
head(hawkesBayDT)

# > Make report ----
# >> yaml ----
version <- "0.5"
title <- paste0("NZ GREEN Grid Household Electricity Demand Data")
subtitle <- paste0("EECA Data Analysis (Part B) GXP Data Report v", version)
authors <- "Ben Anderson"


# >> run report ----
rmdFile <- paste0(repoParams$repoLoc, "/dataProcessing/gxpReport.Rmd")
makeReport(rmdFile)


