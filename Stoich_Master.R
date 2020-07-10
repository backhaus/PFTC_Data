
##### Creating Master file of all Stoichiometry Data #####

# INSTALL LIBRARIES
install.packages("devtools")
install.packages("googlesheets4")
install.packages("Rtools")
install.packages("tibble")
install.packages("data.table")
install.packages("dplyr")
install.packages("tibble")

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
library("tibble")

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



#### PHOSPHORUS DATA #####
file_out <- NULL
# cor.fact <- tibble(correction.factor = 0, problem = "NA", file.name = "NA")
# testing
sub.p.files <- p.files[c(56:94),]
sub.p.files <- p.files

cor.fact <- matrix(nrow = length(sub.p.files$name), ncol = 3)
names <- c("correction.factor", "problem", "file.name")
colnames(cor.fact) <- names
cor.fact <- as.data.frame(cor.fact)
# 

for(i in 1:max(length(sub.p.files$name))){
  file <- read_sheet(sub.p.files$id[i])
  
  if (dim(file)[1] == 90){
    
    if(is.na(file[[89,16]])){
       cor.fact[i,2] <- "File has no data"
    } else{
     
    cor.fact[i,1] <- file[[89,16]]
    cor.fact[i,3] <- sub.p.files$name[i]
    
      if(cor.fact[i,1] > 1.50){
         cor.fact[i,2] <- "Greater than 1.50"
      } else{}
    
      if(cor.fact[i,1] < 0.85){
         cor.fact[i,2] <- "Less than 0.85"
      } else{}
    
    }
    
    file <- file[10:90, c(1:2, 5, 15:18)]
    colnames(file) <- file[2,]
    
    file <- file[which(file$`list("Column")` == "C"),]
    file <- filter(file, file$SITE != "Hard Red Spring Wheat Flour")
    
    file <- file[,-c(3:4)]
    
    colnames(file)  <- c("Site", "Sample Code", "%P", "P STD DEV", "P CO VAR")
    file$P_filename <- rep(sub.p.files$name[i])
    
    file_out <- rbind(file_out, file)
  }  else{} 
  
}

###### END OF PHOSPHORUS DATA ######

#### CN DATA #####
cn.files <- cn.files[-which(cn.files$name %like% ".xls"),]
cn.files <- cn.files[-which(cn.files$name == "CN_Michaletz2016.2"),]

cn.info <- matrix(nrow = length(cn.files$name), ncol = 4)
names <- c("file.name", "id", "row.length", "column.length")
colnames(cn.info) <- names
cn.info <- as.data.frame(cn.info)

cn_out <- tibble(sample.id = "NA", year = 0, site = "NA",
                 taxon = "NA",  C = 0,  N = 0, CN.ratio = 0, 
                 d15N = 0, d13C = 0, date.processed = "NA", 
                 file.name = "NA")

cn_out.1 <- tibble(sample.id = "NA", site = "NA")

cn.sub <- NULL

for(i in 1:max(length(cn.files$name))){
  file <- read_sheet(cn.files$id[i])
  
  cn.info[i,1] <- cn.files$name[i]
  cn.info[i,2] <- cn.files$id[i]
  cn.info[i,3] <- as.matrix(dim(file))[1,]
  cn.info[i,4] <- as.matrix(dim(file))[2,]
 
  print(cn.files$name[i])
  print(dim(file))
  
}

for(i in 1:max(length(cn.files$name))){
  file <- read_sheet(cn.files$id[i])
 
  if(length(which(file[,5] %like% "CN Worksheet")) != 0){
     file <- file[c(which(file[,2] == "ID"):dim(file)[1]), c(2:3)]
     colnames(file) <- file[1,]
     file <- file[-1,]
     
     if(length(which(file[,1] == "NULL")) != 0){
        file <- file[-c(which(file[,1] == "NULL")),]
     } else{
        file <- file[-c(which(is.na(file[,1]))),]
     }
     
     file.names <- c("sample id", "site")
     colnames(file) <- file.names
  
     cn_out.1 <- rbind(cn_out.1, file)
  }
  
  if(length(which(file[,1] %like% "REPORT OF ANALYSES")) != 0){
     # sub.date.1 <- substr(file[[6,9]], 1,  3)
     # sub.date.2 <- substr(file[[6,9]], 10, 17)
     # sub.date.2 <- paste(sub.date.1, sub.date.2)
     # cn.date <- as.Date(sub.date.2, format = "%B %d, %Y")
     
     file <- file[c(which(file[,1] == "Sample" | file[,2] == "Sample ID"):dim(file)[1]), c(1:11)]
     colnames(file) <- file[1,]
     
     file <- file[-c(which(file[,1] == "NULL")),]
     file <- file[!grepl("NA", names(file))]
     
     if(colnames(file[,1]) == "list(NULL)"){
        file.names <- c("remove.1", "Sample ID", "Site", "remove.2", "remove.3", "%C", "%N", "CN ratio",
                        "d15N", "d13C")
        colnames(file) <- file.names
        file <- file[!grepl("remove", names(file))]
     }
     
     if(length(which(file[,1] == "Sample")) != 0){
        file <- file[-c(which(file[,1] == "Sample")),]
       
       # Testing if first column is only numbers 1-96 for the samples or the sample code
       if(file[[5,1]] == 5){
          file.names <- c("remove.1", "sample ID", "Site", "remove.2", "remove.3", "%C", "%N", "CN ratio",
                          "d15N", "d13C")
          colnames(file) <- file.names
          file <- file[!grepl("remove", names(file))]
       }
     }
     
     if(length(which(file[,2] == "Sample ID")) != 0){
        file <- file[-c(which(file[,2] == "Sample ID")),]
     }
     
     file <- file[-c(which(is.na(file[,1]))),]
    
     
   cn_out <- rbind(cn_out, file) 
  }
}

file <- read_sheet(cn.files$id[i])

i=8
file <- read_sheet(cn.info[8,2])

dim(file)

# CN files pre-Isotope lab have dim of 108 rows and 8 columns

# CN files post-Isotope lab have dim of x rows and greater than 9 columns


# https://stackoverflow.com/questions/47851761/r-how-to-read-a-file-from-google-drive-using-r

# - Change XLSX files to Google Sheets (all)
#     - try to avoid this, keep searching for another way
# - Make a function that extracts CN data then another function to extract P data
# - put checks in the functions to make sure when run again, duplication does not happen
# - use join to combine the two data sets where there are the same site and sample ID
