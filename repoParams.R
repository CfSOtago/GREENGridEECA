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
repoParams$support <- paste0(repoParams$repoLoc, "/includes/support.Rmd")
repoParams$data <- paste0(repoParams$repoLoc, "/includes/data.Rmd")
repoParams$history <- paste0(repoParams$repoLoc, "/includes/history.Rmd")
repoParams$citation <- paste0(repoParams$repoLoc, "/includes/citation.Rmd")


# Parameters you should _not_ need to change (as they are only used to process the original non-released data) ----
## Location of original data ----

repoParams$dataLoc <- "~/greenGridData/cleanData/safe" # HCS mounted on Computer.Sci RStudio server by default
repoParams$gridSpy <- paste0(repoParams$dataLoc, "/gridSpy/1min/data/")
