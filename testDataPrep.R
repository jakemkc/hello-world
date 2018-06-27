.packages <- c("data.table", "dplyr", "readr", "ggplot2", "ggmap", "readstata13", "log4r")
# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Load packages into session
lapply(.packages, library, character.only=TRUE)

setwd(Sys.getenv("DHS-India"))

source("./utilFunctions.R")

# set up logger
logger <- create.logger()
logfile(logger) <- file.path('log/checkResults.log')

level(logger) <- "INFO"

## read test file
hh_test <- readRDS("./data/testCase.rds")
## check for complete cases
length(which(complete.cases(hh_test))) #0

## check missing data - takes a long time may have a bug!!
# missingPlot(hh_test)

dat <- dataPreparation(hh_test, 0.9, logger)
