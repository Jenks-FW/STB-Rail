##--------------- Header --------------------------------------------
## Script name: New STB Data Cleanup
## Purpose: Gather and clean all new rail data
## Author: Brad Jenkins
## Date Created: 2019-12-19
## Email: BJenkins@FreightWaves.com
##--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
## Notes:
##   This script was created to download new rail data quarterly and annually from AAR.org, convert to CSV file,
##  import, clean it up, and send it to a SQL Database.
##
##--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


## --------------- Initialize ---------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, magrittr)
memory.limit(30000000)   # Needed on some PCs to increase memory allowance, no impact on macs


## --------------- Sources ------------------------------------------

# FILENAMES MUST INCLUDE COMPANY, QUARTER, AND YEAR 
# ex: "C:/.../documents/BNSF_Q3_2019.xls"
raw_dataPath <- 
  "C:/Users/bjenkins/Documents/Datasets/STB-Data/STB-Raw-New/"
clean_dataPath <-
  "C:/Users/bjenkins/Documents/Datasets/STB-Data/STB-Clean-Data/"


## --------------- Functions ----------------------------------------

# Remove commas, change data_value columns from 'character' to 'numeric'
fix.numeric <- function(DF) {
  DF %>%
    mutate_at(vars(2:12), str_remove_all, pattern = ",") %>%
    # leaves negative values, pattern = "^-\\d+$" removes them
    mutate_at(vars(2:12), str_replace_all, pattern = "^\\s*-\\s*$", "0") %>% 
    mutate_at(vars(2:12), as.numeric) %>%
    mutate_if(is.numeric, ~ replace(., is.na(.), 0))
}

# Separate Commodity ID from Description
split.id <- function(DF, com_col = 1) {
  DF %>%
    rename(com_orig = com_col) %>%
    mutate(com_id = str_extract(com_orig, "^\\d+")) %>%
    select(com_id, everything()) %>%
    select(-com_orig)
}

# Remove commas, quotes, rows of NAs, etc. from com_id
clean.id <- function(DF) {
  DF %>%
    # If id's have anything but digits, it will need to be removed first
    mutate_at(vars(com_id), str_remove_all, pattern = "'|,") %>%
    mutate_at(vars(com_id), str_trim, side = "both") %>%
    # Remove rows that don't start with com_id specific digits
    filter(str_detect(com_id, pattern = "^(0|1|2|3|4|8|9)\\d*$")) #%>% 
    #filter(!str_detect(com_id, pattern = "^\\s*0\\s*$")) # Should I drop id=0 from com_id?
}

# Add leading zero where they're missing
pad.left <- function(DF, char = '0', stop_row = '10') {
  DF %>%
    # Create new group_id when you find com_id = 1
    mutate(grp = cumsum(str_detect(com_id, "^1$|^01$"))) %>%
    group_by(grp) %>%
    # Mutate if the ID is missing the leading 0
    mutate(com_id = ifelse(
      !str_detect(com_id, '^0') &
        row_number() <= which(lead(com_id == stop_row)),
      str_pad(com_id, str_length(com_id) + 1, 'left', char),
      com_id
    )) %>%
    ungroup() %>%
    select(-grp)
}

# Add Descriptions for official com_id values, NA for unofficial ones
add.desc <- function(DF){ DF %>%
    left_join(AAR_Com_Code %>% select(STCC, Traditional.FCS.Name), by = c('com_id' = 'STCC')) %>%
    select(com_id, com_desc = Traditional.FCS.Name, everything())
}


## --------------- Import new rail Excel files ----------------------

# Create list of excel filenames in directory
xl_files <-
  list.files(raw_dataPath, pattern = "\\.xlsx?", full.names = TRUE)

rail_pattern <- "(BNSF)|(CSX)|(GTC)|(KCS)|(NS)|(SOO)|(UP)"

# Loop over xl files to import as list of tibbles
DF_list <- NULL
for (i in 1:length(xl_files)) {
  DF_list <- map_df(excel_sheets(xl_files[i]),
                    ~ read_excel(xl_files[i],
                                 sheet = .x, 
                                 col_names = FALSE),) %>% 
    list() %>% 
    set_names(., str_extract(xl_files[i], rail_pattern)) %>% 
    append(DF_list, .)
}

rail_master <- 
  enframe(names(DF_list), name = NULL) %>%
  mutate(
    filepath = xl_files,
    raw_df = DF_list
  )


## --------------- Import CSV files ---------------------------------

# Load AAR Commodity Codes
AAR_Com_Code <-
  read.csv(
    file = paste0(clean_dataPath, "AAR-Commodity-Codes.csv"),
    header = TRUE,
    colClasses = "character"
  ) %>% 
  as_tibble()


## --------------- Check that all files imported --------------------

if (!all(names(DF_list) %in% c("BNSF", "CSX", "GTC", 
                               "KCS", "NS", "SOO","UP"))) {
  stop('One or more datasets is missing!')
}

#EXAMPLE
#!all(c('a', 'b', 'c') %in% c('a', 'b', 'c', 'd')) ## FALSE
#!all(c('a', 'b', 'c', 'd') %in% c('a', 'b', 'c')) ## TRUE


## ---------- Extract DFs, Name columns, drop extras ----------------

for (i in 1:length(DF_list)) {
  # Temp assign DF
  DF <- DF_list[[i]]
  if (names(DF_list[i]) == "SOO") {
    assign(names(DF_list[i]), select(
      DF,
      com_id = 4,
      y = c(5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 19)))
  } else if (names(DF_list[i]) == "UP") {
    keep_col <- colSums(is.na(DF)) / nrow(DF) < 0.95
    DF <- DF[, keep_col, drop = FALSE]
    assign(names(DF_list[i]), select(
      DF,
      com_id = 1, 
      y = 2:12))
  } else if (names(DF_list[i]) == "GTC" | DF_list[i] == "NS") {
    assign(names(DF_list[i]), select(
      DF,
      com_id = 1,
      y = 3:13))
  } else {
    assign(names(DF_list[i]), select(
      DF,
      com_id = 1,
      y = 2:12))
  }
}

# UP Example:
#UP18_annual <- read.csv(file.choose(new = TRUE), 
#                        header = FALSE, 
#                        stringsAsFactors = FALSE, 
#                        na.strings=c(""," ","NA"))
#keep_col <- colSums(is.na(UP18_annual)) / nrow(UP18_annual) < 0.95
#UP18_annual <- UP18_annual[, keep_col, drop = FALSE]
#UP18_annual %<>%  select(com_id = 1, y = 2:12)
#View(UP18_annual)


## --------------- Tidy the Datasets --------------------------------

id_desc <- "\\s*\'?0?1\\s*FARM\\s*PRODUCTS\\s*"
stop_row_id <- c('10', '101', '10112', '102')

for (i in 1:length(DF_list)) {
  # Temp assign DF
  DF <- eval(as.name(rail_master$value[i])) %>% 
    # Remove comma, dash, NA, etc from columns, change class to numeric
    fix.numeric() %>% 
    # Separate id/description if combined, drop description col
    when(select(., com_id) %>% str_detect(id_desc)
         ~split.id(.),
         ~.) %>% 
    # Remove comma, quote, NA, etc from com_id column
    clean.id() 
  
  # Determine which row will stop pad.left function
  stop_row <- DF %>% select(com_id) %>% 
    as.character() %>% str_detect(pattern = fixed(stop_row_id)) 
  
  # Based on stop_row, pad.left until stop_row_id reached
  if (stop_row[1]) {
    DF <- pad.left(DF, stop_row = stop_row_id[1])
  } else if (stop_row[2]) {
    DF <- pad.left(DF, stop_row = stop_row_id[2])
  } else if (stop_row[3]) {
    DF <- pad.left(DF, stop_row = stop_row_id[3])
  } else if (stop_row[4]) {
    DF <- pad.left(DF, stop_row = stop_row_id[4])
  } else {
    stop('Can\'t identify stop_row for one or more datasets!')
  } 
  
  DF %<>%
    # Add STCC descriptions to all official com_id's 
    add.desc() %>%
    # Add com_verify column to mark unofficial com_id's
    mutate(com_verify = DF$com_id %in% AAR_Com_Code$STCC,
           company = names(DF_list[i]),
           quarter = str_match(xl_files[i], 
                               "(Q\\d{1})_\\d{4}.xlsx?")[,2],
           year = str_match(xl_files[i], 
                            "Q\\d{1}_(\\d{4}).xlsx?")[,2],
           ) %>% 
    select(company, year, quarter, everything())
  
  # Give columns descriptive names
  colnames(DF) <- c("company",
                    "year",
                    "quarter",
                    "commodity_id",
                    "commodity_description",
                    "original_terminate_carloads",
                    "original_terminate_tons",
                    "original_deliver_carloads",
                    "original_deliver_tons",
                    "received_terminate_carloads",
                    "received_terminate_tons",
                    "received_deliver_carloads",
                    "received_deliver_tons",
                    "total_carried_carloads",
                    "total_carried_tons",
                    "total_gross_revenue",
                    "commodity_verify"
                    )
  
  # Assign temp variable back to rail company
  assign(names(DF_list[i]), DF)
} 
## --------------- WORK IN PROGRESS --------------------------------- 


## --------------- Validation -------------------------------------

# AAR codes missing from each DF
missing_AAR <- vector('list', 7)
for (i in 1:length(DF_list)) {
  DF <- eval(as.name(rail_master$value[i]))
  missing_AAR[[i]] <- AAR_Com_Code %>% 
    filter(!AAR_Com_Code$STCC %in% DF$commodity_id)
  #missing_AAR[[i]] <- tmp
  #(paste0(DF_list[i], "-missing_AAR"))
}

# Add new col of clean DFs to rail_master
rail_master %<>% 
  mutate(clean_df = lst(BNSF, CSX, GTC, KCS, NS, SOO,UP), 
         missing_AAR_codes = missing_AAR)

rail_new <- bind_rows(BNSF, CSX, GTC, KCS, NS, SOO, UP)


## --------------- Export -------------------------------------------

#dbWriteTable(scon, 
#             "indx_index_data", 
#             ???????,
#             overwrite = F,
#             append = T)
#Don't forget to move/delete excel files to not get picked up again
##