# Package parameters ----

repoParams <<- list() # params holder

# Parameters you may need to change ----

# Location of the repo
library(here)
repoParams$repoLoc <- here::here()

# Location of misc data
repoParams$dstNZDates <- paste0(repoParams$repoLoc, "/data/dstNZDates.csv")

# Vars for Rmd
repoParams$pubLoc <- "[Centre for Sustainability](http://www.otago.ac.nz/centre-sustainability/), University of Otago: Dunedin"
repoParams$Authors <- "Anderson, B., Dortans, C, Mair, J. and Jack, M."

# Rmd includes
repoParams$licenseCCBY <- paste0(repoParams$repoLoc, "/includes/licenseCCBY.Rmd")
repoParams$supportGeneric <- paste0(repoParams$repoLoc, "/includes/supportGeneric.Rmd")
repoParams$sampleGeneric <- paste0(repoParams$repoLoc, "/includes/sampleGeneric.Rmd")
repoParams$history <- paste0(repoParams$repoLoc, "/includes/historyGeneric.Rmd")
repoParams$citation <- paste0(repoParams$repoLoc, "/includes/citationGeneric.Rmd")


# Parameters you should _not_ need to change (as they are only used to process the original non-released data) ----
## Location of original data ----

repoParams$dataLoc <- "/Volumes/hum-csafe/Research Projects/GREEN Grid/" # HCS by default
