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


## --------------- Convert new rail XL to CSV -----------------------

# Create list of excel filenames in directory
xl_files <-
  list.files(raw_dataPath, pattern = "\\.xlsx?", full.names = TRUE)

rail <- "(BNSF)|(CSX)|(GTC)|(KCS)|(NS)|(SOO)|(UP)"

# Loop over xl_files creating a CSV for each
for (i in 1:length(xl_files)) {
  xl_sheets <- excel_sheets(xl_files[i])
  rail_xl <- map_df(xl_sheets,
                    ~ read_excel(xl_files[i],
                                 sheet = .x, 
                                 col_names = FALSE),) %>%
    write_csv(path = paste0(
      raw_dataPath,
      "raw-",
      str_extract(xl_files[i], rail),
      ".csv"
    ))
}

# Clean up
rm(rail, rail_xl, xl_sheets, xl_files)


## --------------- Import CSV files ---------------------------------

# Create list of CSV files
csv_filenames <-
  list.files(path = raw_dataPath, 
             pattern = ".*.csv",
             full.names = FALSE)

rail_master <- 
  enframe(csv_filenames, name = NULL) %>%
  mutate(
    filepath = str_c(raw_dataPath, value, sep = ''),
    raw_df = map(filepath, read_csv)
  )

# Load AAR Commodity Codes
AAR_Com_Code <-
  read.csv(
    file = paste0(clean_dataPath, "AAR-Commodity-Codes.csv"),
    header = TRUE,
    colClasses = "character"
  ) %>% 
  as_tibble()

# Load list of expected filenames
always_check <-
  read_lines(file = paste0(clean_dataPath, "always_check.csv"))


## --------------- Check that all files imported --------------------

if (!all(always_check %in% rail_master$value)) {
  stop('One or more datasets is missing!')
}


## --------------- Extract df for each rail company -----------------

DF_list <- NULL
for (i in 1:length(csv_filenames)) {
  rail_name <- str_sub(csv_filenames[i], start = 5L, end = -5L)
  assign(rail_name,
         as_tibble(rail_master$raw_df[i], .name_repair = "minimal")[[1]])
  DF_list <- append(DF_list, rail_name)
}


## --------------- Name columns, drop extras ------------------------

for (i in 1:length(DF_list)) {
  # Temp assign DF
  DF <- eval(as.name(DF_list[i]))
  if (DF_list[i] == "SOO") {
    assign(DF_list[i], select(
      DF,
      com_id = 4,
      y = c(5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 19)))
  } else if (DF_list[i] == "UP") {
    keep_col <- colSums(is.na(DF)) / nrow(DF) < 0.95
    DF <- DF[, keep_col, drop = FALSE]
    assign(DF_list[i], select(
      DF,
      com_id = 1, 
      y = 2:12))
  } else if (DF_list[i] == "GTC" | DF_list[i] == "NS") {
    assign(DF_list[i], select(
      DF,
      com_id = 1,
      y = 3:13))
  } else {
    assign(DF_list[i], select(
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

## --------------- WORK IN PROGRESS --------------------------------- Works for BNSF, not for CSX 

for (i in 1:length(DF_list)) {
  # Temp assign DF
  DF <- eval(as.name(DF_list[i])) %>% 
    # Remove comma, dash, NA, etc from data_values columns, change class to numeric
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
    stop('Can\'t identify stop_row to pad leading zero for one or more datasets!')
  } 
  
  DF %<>%
    # Add STCC descriptions to all official com_id's 
    add.desc() %>%
    # Add com_verify column to mark unofficial com_id's
    mutate(com_verify = DF$com_id %in% AAR_Com_Code$STCC)
  
  # Give columns descriptive names
  colnames(DF) <- c("com_id",
                  "com_desc",
                  "orig_terminate_carloads",
                  "orig_terminate_tons",
                  "orig_deliver_carloads",
                  "orig_deliver_tons",
                  "recv_terminate_carloads",
                  "recv_terminate_tons",
                  "recv_deliver_carloads",
                  "recv_deliver_tons",
                  "tot_carried_carloads",
                  "tot_carried_tons",
                  "tot_gross_revenue",
                  "com_verify")
  
  # Assign temp variable back to rail co name
  assign(DF_list[i], DF)
}


## --------------- Validation -------------------------------------

# AAR codes missing from each DF
missing_AAR <- vector('list', 7)
for (i in 1:length(DF_list)) {
  DF <- eval(as.name(DF_list[i]))
  tmp <- AAR_Com_Code %>% 
    filter(!AAR_Com_Code$STCC %in% DF$com_id)
  missing_AAR[[i]] <- tmp
  #(paste0(DF_list[i], "-missing_AAR"))
}

# Add new col of clean DFs to rail_master
rail_master %<>% 
  mutate(clean_df = lst(BNSF, CSX, GTC, KCS, NS, SOO,UP), 
         missing_AAR_codes = missing_AAR)

#add DF summary column?


## --------------- Export -------------------------------------------


#Don't forget to mark XL and CSVs to not get picked up again (or move to another directory)
##