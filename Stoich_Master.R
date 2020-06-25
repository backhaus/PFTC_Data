
##### Creating Master file of all Stoichiometry Data #####

# INSTALL LIBRARIES
install.packages("devtools")
install.packages("googlesheets4")
install.packages("Rtools")
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

# NOTE: FILES MUST BE GOOGLE SHEETS

## get all Google spreadsheets in folder 'IsotopeData'
## whose names contain the letters 'Enquist'
data <- drive_ls(path = "IsotopeData", pattern = "Enquist", type = "spreadsheet")
data$id

data2 <- drive_ls(path = "Stoich2012/PR2012/CN", pattern = "Enquist")

filep <- drive_download(data2$name[1])
drive_get(data$id)
(downloaded_file <- drive_download(data2$id[1]))

downloaded_file$drive_resource$exportLinks$`application/vnd.oasis.opendocument.spreadsheet`[1]

filep <- read_sheet(data$id[1])
fileq <- read_sheet(data2$id[1])
fileq <- read_xlsx(path = data2$name[1])
# https://stackoverflow.com/questions/47851761/r-how-to-read-a-file-from-google-drive-using-r

# - Change XLSX files to Google Sheets (all)
#     - try to avoid this, keep searching for another way
# - Make a function that extracts CN data then another function to extract P data
# - put checks in the functions to make sure when run again, duplication does not happen
# - use join to combine the two data sets where there are the same site and sample ID
