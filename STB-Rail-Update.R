## ----------------- Header --------------------------------------
## Script name: New STB Data Cleanup
## Purpose: Gather and clean all new rail data
## Author: Brad Jenkins
## Date Created: 2019-12-19
## Email: BJenkins@FreightWaves.com
## --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
## Notes:
##   This script was created with the intention to download new rail data quarterly and annually from AAR.org, convert to CSV file, 
##  import as a DF, clean it up, and send it to a SQL Database.
##
## --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


## --------------- 0 - Initialize --------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, magrittr)
options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.


## --------------- 1 - Sources -----------------------------------
raw_dataPath <- "C:/Users/bjenkins/Documents/Datasets/STB-Data/STB-Raw-New/" 
AAR_dataPath <- "C:/Some/Directory/AAR-Commodity-Code.csv"
clean_dataPath <- "C:/Some/Clean/Data/Directory/"
#source(paste0(dataPath,dataFile))
#setwd()


## --------------- 2 - Functions --------------------------------
# Why isn't this already a thing?
`%!in%` <- Negate(`%in%`)

# # Remove commas, change appropriate columns from 'character' to 'numeric'
# fix.numeric <- function(DF){ DF %>%
#     # MUST REMOVE ANY EXTRA COLUMNS BEFORE RUNNING
#     mutate_at(colnames(DF[, (ncol(DF) - 10):ncol(DF)]), # refers to last 11 columns 
#               str_remove_all, 
#               pattern = ",") %>% 
#     mutate_at(colnames(DF[, (ncol(DF) - 10):ncol(DF)]), 
#               str_replace_all, 
#               pattern = "-", 
#               "0") %>%
#     mutate_at(colnames(DF[, (ncol(DF) - 10):ncol(DF)]), 
#               as.numeric) %>% 
#     mutate_if(is.numeric, 
#               ~replace(., is.na(.), 0))
# }



## --------------- 3 - Body -------------------------------------
# Download new rail xl files?


# Convert to CSV
xl_files <- list.files(raw_dataPath, pattern = "\\.xlsx?", full.names = T)
all_rail_xl <- NULL # Initialize all_rail_xl since map_df throws an error 
for (i in 1:length(xl_files)) {
  xl_sheets <- excel_sheets(xl_files[i])
  all_rail_xl <- map_df(
    xl_sheets,
    ~ read_excel(xl_files[i], sheet = .x, col_names = F),
  ) %>% 
    write_csv(path = paste0(raw_dataPath, "CSV-", str_extract(xl_files[i], "(BNSF)|(CSX)|(GTC)|(KCS)|(NS)|(SOO)|(UP)"), ".csv"))
  rm(all_rail_xl)
}

# Import CSV
csv_filenames <- list.files(path = raw_dataPath, pattern = ".*.csv", full.names = F)

rail_list <- list.files(path = raw_dataPath, pattern = ".*.csv", full.names = T) %>%
  set_names(csv_files) %>% 
  lapply(read.csv, header = F, stringsAsFactors = F, na.strings=c("","\\s+","NA"))

BNSF <- as_tibble(rail_list$`CSV-BNSF.csv`)
# Get rid of all columns but ID/ID-Desc and y1:y11 (carloads,tons,revenue)
# Not all sheets have headers, so keep all columns when there's less than 15
headers <- "CODE|STCC|FARM PRODUCTS|CARS?|LOADS?|TONS?|COMMODITY|REVENUE|DOLLARS|TOT|REV"
rail_list <- lapply()
if (ncol(railDF) > 14) { railDF %<>%
  select(# If there's no column headers and more than 14 columns, I have no idea how to automate this
         grep(pattern = headers, railDF, ignore.case = T),
         -str_which(railDF, pattern = "DESCRIPTION"))
}

## --- --- ---
# DANGEROUS TO USE WHEN THERE'S LEADING COLUMNS OF CRAP OR NAs THAT'LL CAUSE DATA TO SHIFT TO WRONG COLUMNS
#railDF <-  as.tibble(t(apply(railDF, 1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} ))) 
## --- --- ---

cond <- colSums(is.na(railDF))/nrow(railDF) < 0.90
railDF <- railDF[, cond, drop = F]


# Clearing commas out, replacing '-' with '0', and changing class to numeric for all measured data
# MUST REMOVE ANY EXTRA COLUMNS BEFORE RUNNING
railDF <- railDF %>%
  mutate_at(colnames(DF[, (ncol(DF) - 10):ncol(DF)]), # references last 11 columns 
            str_remove_all, 
            pattern = ",") %>% 
  mutate_at(colnames(DF[, (ncol(DF) - 10):ncol(DF)]), 
            str_replace_all, 
            pattern = "-", 
            "0") %>%
  mutate_at(colnames(DF[, (ncol(DF) - 10):ncol(DF)]), 
            as.numeric) %>% 
  mutate_if(is.numeric, 
            ~replace(., is.na(.), 0))


## --------------- 4 - Verification -----------------------------
#Verify it!

## --------------- 5 - Export -----------------------------------
#Send it somewhere!

##