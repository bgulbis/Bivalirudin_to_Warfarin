##
## library.R
##
## all libraries needed to perform analysis
##

library(BGTools)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(R.utils)
library(zoo)
library(MESS)
# library(FSA)
# library(pander)
library(ggplot2)
library(grid)
library(RColorBrewer)
# library(ReporteRs)
# library(export)

data.dir <- "Data"
manual.dir <- "Manual"
analysis.dir <- "Analysis"

gzip_files(data.dir)
