#################################
### Enquist lab CNP workflow ###
#################################

# INSTALL LIBRARIES
install.packages("tidyverse")
install.packages("lubridate")
install.packages("googlesheets4")
install.packages("readxl")
install.packages("R.utils")
install.packages("broom")
install.packages("googledrive")
install.packages("glue")
install.packages("dplyr")
install.packages("devtools")
install.packages("curl")
install.packages("gtools")
install.packages("data.table")
install.packages("remotes")
install.packages("purrr")
install.packages("plyr")

remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")



# LOAD LIBRARIES
library("tidyverse")
library("lubridate")
# devtools::install_github("tidyverse/googlesheets4")
library("googlesheets4")
library("readxl")
library("R.utils")
library("broom")
library("googledrive")
library("glue")
library("tidyr")
library("dplyr")
library("curl")
library("gtools")
library("gtools")
library("data.table")
library("remotes")
library(PFTCFunctions)
library("purrr")


pn <- . %>% print(n = Inf)


############################################################################
#### ENVELOPE CODES ####
get_PFTC_envelope_codes <- function(seed){
  if (getRversion() < "3.6.0") {
    set.seed(seed = seed)
  }
  else {
    suppressWarnings(set.seed(seed = seed, sample.kind = "Rounding"))
  }
  all_codes <- crossing(A = LETTERS, B = LETTERS, C = LETTERS) %>% 
    mutate(code = paste0(.data$A, .data$B, .data$C), hash = row_number()%%10000L, 
           hash = sample(.data$hash), hash = formatC(.data$hash, 
                                                     width = 4, format = "d", flag = "0"), 
           hashcode = paste0(.data$code, .data$hash)) %>% select(.data$hashcode)
  return(all_codes)
}

# Create list with all valid IDs per country
creat_ID_list <- function(envelope_codes){
  all_codes <- get_PFTC_envelope_codes(seed = 1) %>% 
    mutate(Site = "Peru") %>% 
    bind_rows(get_PFTC_envelope_codes(seed = 32) %>% 
                mutate(Site = "Svalbard"))
  return(all_codes)
}


############################################################################
#### PHOSPHORUS DATA ####
# Download Phosphorus data from google sheet
import_phosphorus_data <- function(){
  cnp <- drive_get("CNP_Template")
  p <- read_sheet(ss = cnp, sheet = "Phosphorus") %>% as_tibble()
  return(p)
}


##### Aud's updated code for this function #####
  # pull of standard, calculate R2, choose standard for absorbance curve, make regression and plot
  get_standard <- function(p){
    standard_concentration <- tibble(Standard = c(0, 2, 4, 8, 12, 16),
                                     Concentration = c(0, 0.061, 0.122, 0.242, 0.364, 0.484))

    Standard <- p %>%
      select(Site, Batch, Individual_Nr, Sample_Absorbance) %>%
      filter(Individual_Nr %in% c("Standard1", "Standard2"),
             # remove batch if Sample_Absorbance is NA; Sample has not been measured
             !is.na(Sample_Absorbance)) %>%
      group_by(Site, Batch, Individual_Nr) %>%
      # not a good solution!!!
      mutate(n = n()) %>%
      filter(n == 6) %>%
      nest(standard = c(Sample_Absorbance)) %>%
      mutate(standard = map(standard, bind_cols, standard_concentration))

    return(Standard)
  }


# Plot 2 Standard curves
# make sure this does it by Site as well - double check this bit
plot_standards <- function(Standard){
  p1 <- Standard %>% 
    unnest(cols = c(standard)) %>% 
    ggplot(aes(x = Sample_Absorbance, y = Concentration, colour = Individual_Nr)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = "Absorbance", y = expression(paste("Concentration ", mu, "g/ml")))
  
  return(p1)
}


# Choose standard and make model
standard_model <- function(Standard){
  ModelResult <- Standard %>% 
    mutate(correlation = map_dbl(standard, ~cor(.$Sample_Absorbance, .$Concentration, use = "pair"))) %>% 
    group_by(Site, Batch) %>% 
    slice(which.max(correlation)) %>% 
    mutate(fit = map(standard, ~lm(Concentration ~ Sample_Absorbance, .)))
  return(ModelResult)
}


# Calculate Mean, sd, coeficiant variability for each leaf and flag data
original_phosphor_data <- function(p, ModelResult){
  # p2 <- filter(p2, !is.na(Site))
  p2 <- p %>% 
    filter(!Individual_Nr %in% c("Standard1", "Standard2"),
           # remove samples without mass
           !is.na(Sample_Mass)) %>% 
    group_by(Site, Batch) %>% 
    # nest_legacy(.key = "data") %>% 
    nest_legacy(.key = "data") 
  
    p2$Site  <- as.factor(p2$Site)
    p2$Batch <- as.factor(p2$Batch)
    ModelResult$Site  <- as.factor(ModelResult$Site)
    ModelResult$Batch <- as.factor(ModelResult$Batch)
    
    p2 <- left_join(ModelResult %>% select(-Individual_Nr), p2, by = c("Site", "Batch"))
 
    # remove if correlation is NA, but give warning
    # if(nrow(p2 %>% filter(is.na(correlation))) > 0){
    #   p2 <- p2 %>%
    #     filter(!is.na(correlation))
    # }

  # to remove Batches that were incomplete, either with only one standard or less than 6 replicates for a standard
               
  OriginalValues <- p2 %>% 
      mutate(data = map2(.x = data, .y = fit, ~ mutate(.x, Sample_yg_ml = predict(.y, newdata = select(.x, Sample_Absorbance))))) %>% 
  # unnest function is bringing up an error. Names must be unique "Site" at locations 2 and 15 are duplicated
    # probably with Peru and Svalbard being the only two levels... not sure how to fix it.
    unnest(data) %>%
    mutate(Pmass = Sample_yg_ml * Volume_of_Sample_ml,
           Pconc = Pmass / Sample_Mass * 100) %>% 
    # Calculate mean, sd, coefficient of variation
    group_by(Batch, Site, Individual_Nr) %>% 
    mutate(meanP = mean(Pconc, na.rm = TRUE), 
           sdP = sd(Pconc, na.rm = TRUE),
           CoeffVarP = sdP / meanP) %>% 
    # flag data
    mutate(Flag_orig = ifelse(CoeffVarP >= 0.2, "flag", ""))
  

  return(OriginalValues)
}


# wheat: check values, flag/remove, calculate correction factor
calculate_correction_factor <- function(OriginalValues, RedWheatValue = 0.137){
  
  CorrectionFactor <- OriginalValues %>% 
    filter(Individual_Nr %in% c("Hard Red Spring Wheat Flour")) %>% 
    mutate(P_Correction = Pconc / RedWheatValue) %>% 
    # Calculate mean, sd, coefficient of variation
    group_by(Batch, Site, Individual_Nr) %>% 
    summarise(Correction_Factor = mean(P_Correction, na.rm = TRUE)) %>% 
    select(-Individual_Nr)
  return(CorrectionFactor)
  
}


# Use Correction Factor on data
corrected_phosphor_data <- function(OriginalValues, CorrectionFactor){
  CorrectedValues <- OriginalValues %>% 
    filter(!Individual_Nr %in% c("Hard Red Spring Wheat Flour")) %>% 
    left_join(CorrectionFactor, by = c("Batch", "Site")) %>% 
    mutate(Pconc_Corrected = Pconc * Correction_Factor) %>% 
    # Calculate mean, sd, coefficient of variation
    group_by(Batch, Site, Individual_Nr) %>% 
    mutate(meanP_Corrected = mean(Pconc_Corrected, na.rm = TRUE), 
           sdP_Corrected = sd(Pconc_Corrected, na.rm = TRUE),
           CoeffVarP_Corrected = sdP_Corrected / meanP_Corrected,
           N_replications = n()) %>% 
    # flag data
    mutate(Flag_corrected = ifelse(CoeffVarP_Corrected >= 0.2, "flag", ""))
  return(CorrectedValues)
}


### Check IDs
checkIDs <- function(CorrectedValues, all_codes){
  NotMatching <- CorrectedValues %>% 
    anti_join(all_codes, by = c("Individual_Nr" = "hashcode", "Site" = "Site")) %>% 
      select(Batch, Site, Individual_Nr)
  return(NotMatching)

}

############################################################################
#### CN DATA ####
# download isotope data from google drive
download_isotope_data <- function(){
  path <- "EnquistLab_CNP_Workflow/IsotopeData"
  list_of_files <- drive_ls(path = path, pattern = "REPORT") 
  
  # this map function works to download the files as XLSX (should be CSV) into a local spot on computer
  map(list_of_files$name, drive_download, overwrite = TRUE)
  
  # previous version with list_of_files being only GoogleSheet ID
  # didn't work
   # map(list_of_files, ~drive_download(as_id(.), path = file.path("isotope_data", .), overwrite = TRUE))
   # map(list_of_files$id, ~drive_download(as_id(.), path = file.path("isotope_data", .), overwrite = TRUE))
  # Error is:
      #Error in curl::curl_fetch_disk(url, x$path, handle = handle) : 
      #Failed to open file _______ and then file name it is attempting to download
}

# import CN and isotope data and merge
import_cn_data <- function(import_path_name){
  # CN mass
  # cnp <- gs_title("CNP_Template")
  cnp <- drive_get("CNP_Template")
  cn_mass <- read_sheet(ss = cnp, sheet = "CN") %>% 
    mutate(Samples_Nr = as.character(Samples_Nr)) %>% 
    as_tibble()
  
# read in all Google sheet files by Sheet ID, sometimes this is finnicky and will error but try again,
  # it should work the second time
  path <- "EnquistLab_CNP_Workflow/IsotopeData"
  list_of_files <- drive_ls(path = path, pattern = "REPORT") 
  
  cn_files <- map(list_of_files$id, read_sheet, skip = 13)
  
# create cn_isotopes to hold the data
  cn_holder <- tibble(Samples_Nr = as.character(), Individual_Nr = as.character(),
                      Site = as.character(), Row = as.character(), Column = as.numeric(),
                      C_percent = as.list(NA), N_percent = as.list(NA), CN_ratio <- as.list(NA),
                      dN15_percent = as.list(NA), dC13_percent = as.list(NA), Remark_CN = as.logical(NA))
  
  # go through each and manipulate based on specific dimensions 
  for(i in 1:max(length(cn_files))){
  
     if(dim(cn_files[[i]])[2] <=4){
       next
       # these need to be skipped
       # no usable data
     }
     # larger than 4 columns but less than 12, these do not include extra columns
     if(dim(cn_files[[i]])[2] > 4 & dim(cn_files[[i]])[2] < 12){
       if(dim(cn_files[[i]])[2] == 10){
         cn_isotopes <-  cn_files[[i]] %>% 
                         slice(1:grep("Analytical precision, 1-sigma", ...1)-1) %>% 
                         filter(!is.na(...1)) %>% 
                         rename(Samples_Nr = ...1, Individual_Nr = `Sample ID`, Site = ...3, Row = R, Column = C, 
                                C_percent = `C%` , N_percent = `N%`, CN_ratio = `C/N`, dN15_percent = `δ15N ‰(ATM)`, 
                                dC13_percent = `δ13C ‰(PDB)`#, Remark_CN = ...11
         )
         # add 11th column of Remark_CN for easier join/rbinding
         cn_isotopes$Remark_CN  <- as.logical(NA)
         cn_isotopes$Samples_Nr <- as.character(cn_isotopes$Samples_Nr)
       } # end if( dim == 10)
       if(dim(cn_files[[i]])[2] == 11){
         cn_isotopes <-  cn_files[[i]] %>% 
                         slice(1:grep("Analytical precision, 1-sigma", ...1)-1) %>% 
                         filter(!is.na(...1)) %>% 
                         rename(Samples_Nr = ...1, Individual_Nr = `Sample ID`, Site = ...3, Row = R, Column = C, 
                                C_percent = `C%` , N_percent = `N%`, CN_ratio = `C/N`, dN15_percent = `δ15N ‰(ATM)`, 
                                dC13_percent = `δ13C ‰(PDB)`, Remark_CN = ...11
         ) 
         cn_isotopes$Samples_Nr <- as.character(cn_isotopes$Samples_Nr)
       } # end if(dim == 11)
     } # end if(dim > 4 and < 12)
     
     # these ones have more than 12 columns. The assumption is that any column after 12 will have information that is not needed.
     # the select function excludes the columns greater than 12 up to the total number of columns
     if(dim(cn_files[[i]])[2] >= 12){
       cn_isotopes <-  cn_files[[i]] %>% 
                       select(.,-c(...12:dim(cn_files[[i]])[2])) %>% 
                       slice(1:grep("Analytical precision, 1-sigma", ...1)-1) %>% 
                       filter(!is.na(...1)) %>% 
                       rename(Samples_Nr = ...1, Individual_Nr = `Sample ID`, Site = ...3, Row = R, Column = C, 
                              C_percent = `C%` , N_percent = `N%`, CN_ratio = `C/N`, dN15_percent = `δ15N ‰(ATM)`, 
                              dC13_percent = `δ13C ‰(PDB)`, Remark_CN = ...11
        )  
       cn_isotopes$Samples_Nr <- as.character(cn_isotopes$Samples_Nr)
     } # end if(dim >= 12)
   
    # remove NULL/NA columns
    cn_isotopes <- cn_isotopes %>% filter(., Samples_Nr != "NULL")
    
    # combine into new tibble, easier than doing a join every time
    cn_holder <- rbind(cn_holder, cn_isotopes)
    
  } # end for loop
  
  # all columns must be same type, like numeric or character in both tibbles before joining
  cn_mass$Column <- as.list(cn_mass$Column)
  
  # join cn_holder with cn_mass and create new tibble
  cn_data <- cn_mass %>% 
    full_join(cn_holder, by = c("Samples_Nr", "Individual_Nr", "Site", "Row", "Column")) %>% 
    rename(Row_cn = Row, Column_cn = Column)
  return(cn_data) 
} 

# add date measured
#mutate(date_measured = date)


check_cn_data <- function(cn_data){
  # check not matching ids
  not_matching_ids <- cn_data %>% 
    filter(is.na(Sample_Mass) | is.na(C_percent))
  return(not_matching_ids)
}

# join all tables, and test IDs !!!
merge_cnp_data <- function(cn_data, CorrectedValues){
  cnp_data <- cn_data %>% 
    full_join(CorrectedValues, by = c("Individual_Nr", "Site"))
  return(cnp_data)
}



############################################################################
#### MAKE REPORT ####
make_report <- function(import_path_name, file_name = NULL){
  # if(!missing(batch_nr)){
  #   Standard <- Standard %>% 
  #     filter(Batch == batch_nr)
  # }
  
  # run all functions
  RedWheatValue <- 0.137
  envelope_codes <- import_phosphorus_data()
  all_codes <- creat_ID_list(envelope_codes)
  
  p <- import_phosphorus_data()
  standard <- get_standard(p)
  plot_standards(standard)
  ModelResult <- standard_model(standard)
  
  OriginalValues <- original_phosphor_data(p, ModelResult)
  CorrectionFactor <- calculate_correction_factor(OriginalValues, RedWheatValue = RedWheatValue)
  CorrectedValues <- corrected_phosphor_data(OriginalValues, CorrectionFactor)
  
  # import cn and isotope data, merge the two data sets
  message("Downloading isotope data")
  download_isotope_data()
  
  cn_data <- import_cn_data(import_path_name)
  
  check_cn <- check_cn_data(cn_data)
  
  # merge cnp data and output
  cnp <- merge_cnp_data(cn_data, CorrectedValues)
  
  if(!is.null(file_name)){
    write_csv(x = cnp, path = paste0("cnp_results/", file_name, ".csv"))
  }
  
  standard %>% 
    distinct(Batch) %>% 
    arrange(Batch) %>% 
    slice(1:2) %>% 
    pull(Batch) %>% 
    map(~rmarkdown::render("EnquistLab_CNP_Workflow/EnquistLab_CNP_Workflow.Rmd", params = list(batch_nr = .), output_dir = "EnquistLab_CNP_Workflow/Results", output_file = paste0("Results_CNP_Workflow_", ., ".pdf")))
  
  
}

### To do!!!
# staple all pdfs together
# staplr::staple_pdf(input_directory = "EnquistLab_CNP_Workflow/Results", output_filepath = paste0("EnquistLab_CNP_Workflow/Results/", "Full_pdf.pdf"))