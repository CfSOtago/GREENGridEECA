# gets wholesale EA elec gen data
libs <- c(curl, # pulling data off t'interweb
          data.table, # data munching
          forcats, # for cats
          here) # here. not there
library(GREENGridEECA)
GREENGridEECA::loadLibraries(libs) # should install any that are missing

GREENGridEECA::setup() # set data paths etc

# parameters ----
nDays <- 30

# NZ Electricity Authority generation data
# > set months to refresh ----
months <- c("201501", "201502","201503","201504","201505", "201506", "201507","201508","201509","201510","201511","201512")
#months <- c("201801", "201802","201803","201804","201805", "201806", "201807","201808","201809","201810","201811","201812")
# months <- c("201806","201807")

# > EA GXP data location ----
rDataLoc <- "https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Grid_export/"

# > defn of peak ----
amPeakStart <- hms::as.hms("07:00:00")
amPeakEnd <- hms::as.hms("09:00:00")
pmPeakStart <- hms::as.hms("17:00:00") # see https://www.electrickiwi.co.nz/hour-of-power
pmPeakEnd <- hms::as.hms("21:00:00") # see https://www.electrickiwi.co.nz/hour-of-power

# functions ----
stackedDemandProfilePlot <- function(dt) {
  #nHalfHours <- uniqueN(dt$rDate) * 48
  plotDT <- dt[, .(meanDailyKWh = sum(kWh)/nDays), keyby = .(rTime, Fuel_Code)]
  
  p <- ggplot(plotDT, aes(x = rTime, y = meanDailyKWh/1000000, fill = Fuel_Code)) +
    geom_area(position = "stack") +
    labs(x = "Time of Day",
         y = "Mean GWh per half hour",
         caption = "Source: NZ Electricity Authority generation data for June (winter) 2018")
  return(p)
}


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
                     value.name = "kWh" # energy - see https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD/
  )
  reshapedDT <- setEAGenTimePeriod(reshapedDT) # set time periods to something intelligible as rTime
  reshapedDT <- reshapedDT[, rDate := lubridate::dmy(TRADING_DATE)] # fix the dates so R knows what they are. Would be nice if these matched the Gen data
  reshapedDT <- reshapedDT[, rDateTime := lubridate::ymd_hms(paste0(rDate, rTime))] # set full dateTime
  return(reshapedDT)
}


setEAGenTimePeriod <- function(dt){
  # convert the given time periods (TP1 -> TP48, 49. 50) to hh:mm
  dt <- dt[, c("t","tp") := tstrsplit(Time_Period, "P")]
  dt <- dt[, mins := ifelse(as.numeric(tp)%%2 == 0, "45", "15")] # set to q past/to (mid points)
  dt <- dt[, hours := floor((as.numeric(tp)+1)/2) - 1]
  dt <- dt[, strTime := paste0(hours, ":", mins, ":00")]
  dt <- dt[, rTime := hms::as.hms(strTime)]
  # head(dt)
  dt <- dt[, c("t","tp","mins","hours","strTime") := NULL]  #remove these now we're happy
  return(dt)
}

refreshGXPData <- function(months){
  # assumes months is a list of months of the form "201801" to match EA url
  for(m in months){
    rDataF <- paste0(rDataLoc, m, "_Grid_export.csv")
    print(paste0("Getting, processing and cleaning ", m, " (", rDataF, ")"))
    dt <- getData(rDataF)
    data.table::fwrite(dt, file = paste0(repoParams$GXPData, "/EA_", m, "_GXP_MD.csv"))
  } 
}


# check for EA GXP files ----
getGXPFileList <- function(dPath){
  message("Checking for data files")
  all.files <- list.files(path = dPath, pattern = ".csv")
  dt <- as.data.table(all.files)
  dt[, fullPath := paste0(dPath, all.files)]
  return(dt)
}

# load the EA GXP data ----
loadGXPData <- function(files){
  # https://stackoverflow.com/questions/21156271/fast-reading-and-combining-several-files-using-data-table-with-fread
  # this is where we need drake
  # and probably more memory
  # if this breaks you need to run R/getWholesaleGenData.R
  message("Loading ", nrow(files), " files")
  l <- lapply(files$fullPath, fread)
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


# code ----
refreshGXPData(months) # assumes we just get them all, doesn't check if we have any already. Avoid running this if low bandwith
GXPFiles <- getGXPFileList(repoParams$GXPData) # will be empty if never run before so
if(nrow(GXPFiles) == 0){
  message("No data, refreshing!")
  refreshGXPData(months)
} else {
  message("Yep, we've got (some) data")
  GXPDataDT <- loadGXPData(GXPFiles)
}

table(GXPDataDT$POC) # which one(s) do we need?

head(GXPDataDT)

# find the GXP peaks
top100DT <- head(GXPDataDT[order(-GXPDataDT$kWh)], 100)

plotDT <- top100DT[, .(nDates = .N), keyby = .(rDate, POC)]

ggplot2::ggplot(plotDT, aes(x = rDate, y = POC, fill = nDates)) + geom_tile()
# OK, so that's all Tiwai

top100DT <- head(GXPDataDT[POC != "TWI2201"][order(-GXPDataDT$kWh)], 100)

plotDT <- top100DT[, .(nDates = .N), keyby = .(rDate, POC)]

ggplot2::ggplot(plotDT, aes(x = rDate, y = POC, fill = nDates)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90))

# Let's draw a map
