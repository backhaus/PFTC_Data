
##### Creating Master file of all Stoichiometry Data #####

# INSTALL LIBRARIES
install.packages("devtools")
install.packages("googlesheets4")
install.packages("Rtools")
install.packages("tibble")
install.packages("data.table")
install.packages("dplyr")

# LOAD LIBRARIES

library("devtools")
library("tidyverse")
library("lubridate")
devtools::install_github("tidyverse/googlesheets4")
library("googlesheets4")
library("readxl")
library("R.utils")
library("broom")
library("googledrive")
library("tibble")
library("data.table")
library("dplyr")

# NOTE: FILES MUST BE GOOGLE SHEETS

## get all Google spreadsheets in folder 'IsotopeData'
## whose names contain the letters 'Enquist'

# Use the CNP_ENQUIST MASTER_16July2018 google sheet to compare CN file names and P file names 
# to existing files names in various google drive folders

# find Google sheet ID to read the sheet into RStudio
data <- drive_ls(path = "Lab_Mac_Backup/MASTER FILES", pattern = "CNP_ENQUIST", type = "spreadsheet")
# read the sheet into RStudio based on the ID
file <- read_sheet(data$id[1])

# put the unique file names in a data frame to later use anti_join
master.file.names <- unique(file$`CN FILE NAME`)
master.file.names <- append(master.file.names, unique(file$`P FILE NAME`))
master.file.names <- as.data.frame(master.file.names)

name <- "name"
colnames(master.file.names) <- name

# create large data frame with all available files from both Google Drive folders and subfolders
data <- drive_ls(path = "IsotopeData", pattern = "Enquist", type = "spreadsheet")
data <- rbind(data, drive_ls(path = "Lab_Mac_Backup/MASTER FILES", pattern = "CNP_ENQUIST", type = "spreadsheet"))

data <- rbind(data, drive_ls(path = "Stoich2012/PR2012/CN"         , pattern = "Enquist"))
data <- rbind(data, drive_ls(path = "Stoich2012/PR2012/P"          , pattern = "P_2012" ))
data <- rbind(data, drive_ls(path = "Stoich2012/NIWOT2012/Niwot_CN", pattern = "CN_"    ))
data <- rbind(data, drive_ls(path = "Stoich2012/NIWOT2012/Niwot_P" , pattern = "P_2012" ))
data <- rbind(data, drive_ls(path = "Stoich2012/CR2012/CR_CN_2012" , pattern = "CN_"    ))
data <- rbind(data, drive_ls(path = "Stoich2012/CR2012/CR_P_2012"  , pattern = "P_"     ))
data <- rbind(data, drive_ls(path = "Stoich2012/Co2012/Co_CN_2012" , pattern = "CN_"    ))
data <- rbind(data, drive_ls(path = "Stoich2012/Co2012/Co_P_2012"  , pattern = "P_"     ))

data <- rbind(data, drive_ls(path = "Lisa_Peru Stoich 2013_2014_2015_2016_2017/CN_2013_2014_2015/CN Results"  , pattern = "CN_"))
data <- rbind(data, drive_ls(path = "Lisa_Peru Stoich 2013_2014_2015_2016_2017/Phosphorus_2013_2014_2015_2016", pattern = "P_" ))

data <- rbind(data, drive_ls(path = "Stoich 2016_2017/CN/CN_Peru"        , pattern = "CN_"))
data <- rbind(data, drive_ls(path = "Stoich 2016_2017/P/P_Peru"          , pattern = "P_" ))
data <- rbind(data, drive_ls(path = "Stoich 2016_2017/CN/CN_Macrosystems", pattern = "CN_"))
data <- rbind(data, drive_ls(path = "Stoich 2016_2017/P/P_Macrosystems"  , pattern = "P_" ))

data <- rbind(data, drive_ls(path = "Michaletz-Blonder2016_2017", pattern = "CN_"))

data <- rbind(data, drive_ls(path = "Stoich 2017-2018/COLORADO/CN_Colorado" , pattern = "CN_"))
data <- rbind(data, drive_ls(path = "Stoich 2017-2018/COLORADO/P_Colorado"  , pattern = "P_" ))
data <- rbind(data, drive_ls(path = "Stoich 2017-2018/CHINA/CN_China"       , pattern = "CN_"))
data <- rbind(data, drive_ls(path = "Stoich 2017-2018/CHINA/P_China"        , pattern = "P_" ))
data <- rbind(data, drive_ls(path = "Stoich 2017-2018/Biosphere2/Stoich_Biosphere/CN_Biosphere" , pattern = "CN_"   ))
data <- rbind(data, drive_ls(path = "Stoich 2017-2018/Biosphere2/Stoich_Biosphere/P_Biosphere"  , pattern = "P_2018"))
data <- rbind(data, drive_ls(path = "Stoich 2017-2018/Biosphere2/d180_Macrosystems"))

data <- rbind(data, drive_ls(path = "Stoich 2018-2019/Peru"  , pattern = "P_" ))
data <- rbind(data, drive_ls(path = "Stoich 2018-2019/Peru"  , pattern = "CN_"))
data <- rbind(data, drive_ls(path = "Stoich 2018-2019/Norway", pattern = "P_" ))
data <- rbind(data, drive_ls(path = "Stoich 2018-2019/Norway", pattern = "CN_"))
data <- rbind(data, drive_ls(path = "Stoich 2018-2019/China" , pattern = "P_" ))
data <- rbind(data, drive_ls(path = "Stoich 2018-2019/China" , pattern = "CN_"))

data <- rbind(data, drive_ls(path = "Stoich 2019-2020/William", pattern = "CN_"))
data <- rbind(data, drive_ls(path = "Stoich 2019-2020/RMBL"   , pattern = "CN_"))
data <- rbind(data, drive_ls(path = "Stoich 2019-2020/Peru"   , pattern = "CN_"))
data <- rbind(data, drive_ls(path = "Stoich 2019-2020/Peru"   , pattern = "P_" ))
data <- rbind(data, drive_ls(path = "Stoich 2019-2020/Norway" , pattern = "CN_"))
data <- rbind(data, drive_ls(path = "Stoich 2019-2020/Norway" , pattern = "P_" ))

# remove .xslx or .xls from data[,name]
# data$name <- as.character(sub(".xlsx", "", data$name))
# data$name <- as.character(sub(".xls" , "", data$name))

# missing <- anti_join(master.file.names, data, by = name)
missing <- anti_join(data, master.file.names, by = name)

# identify files that do not have final data in them
missing.ignore <- which(missing$name %like% "emplate" | 
                        missing$name %like% "Test Run"|
                        missing$name %like% "Species ")

# remove template files and Test files etc. from the names in the list that are missing
# from the most current master and are in the Google Drive folders
missing <- missing[-c(missing.ignore),]

# separate the file names into CN, P, etc.
# Each have similar file formats
cn.files <- missing[which(missing$name %like% "CN_"),]

p.files <- missing[ which(missing$name %like% "P_"  ),]
p.files <- p.files[-which(p.files$name %like% "CNP_"),]

master.files <- missing[which(missing$name %like% "CNP_"),]

file_out <- NULL
cor.fact <- NULL

# 
test <- read_sheet(p.files$id[1])
dim(test)

# isolate correction factor, create separate tibble with 
cor.fact <- rbind(cor.fact, test[[89,16]])

test <- test[10:90, c(1:2,5,15:18)]

colnames(test) <- test[2,]
test <- test[which(test$`list("Column")` == "C"),]

test <- filter(test, test$SITE != "Hard Red Spring Wheat Flour")
test <- test[,-c(3:4)]

colnames(test)  <- c("Site", "Sample Code", "%P", "P STD DEV", "P CO VAR")
test$P_filename <- rep(p.files$name[1])

file_out <- rbind(file_out, test)

# https://stackoverflow.com/questions/47851761/r-how-to-read-a-file-from-google-drive-using-r

# - Change XLSX files to Google Sheets (all)
#     - try to avoid this, keep searching for another way
# - Make a function that extracts CN data then another function to extract P data
# - put checks in the functions to make sure when run again, duplication does not happen
# - use join to combine the two data sets where there are the same site and sample ID
