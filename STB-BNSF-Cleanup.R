## ----------------- Header ---------------------------
## Script name: BNSF Rail Data
## Purpose: Cleaning up STB BNSF Rail data
## Author: Brad Jenkins (with help from Brad Hill)
## Date Created: 2019-11-30
## Email: BJenkins@FreightWaves.com
## --- --- --- --- --- --- --- --- --- --- --- --- ---
## Notes:
##   32 Excel spreadsheets; one for each quarter and one annual report from 2013 to mid 2019
##   There's 465 official AAR Commodity Codes, BNSF has 470 per report
## --- --- --- --- --- --- --- --- --- --- --- --- ---


## ------------- 0 - Initialize ----------------------
library(tidyverse)
library(readxl)
library(magrittr)


## ------------- 1 - Sources -------------------------
raw_dataPath <- "C:/Users/bjenkins/Documents/Datasets/STB-Data/bnsf/"
AAR_dataPath <- "C:/Users/bjenkins/Documents/Datasets/STB-Data/AAR-Commodity-Code.csv"
clean_dataPath <- "C:/Users/bjenkins/Documents/Datasets/STB-Data/STB-Clean-Data/"
#dataFile <- "some_functions.R" 
#source(paste0(dataPath,dataFile))


## ------------- 2 - Functions ----------------------
# Remove commas, change appropriate columns from 'character' to 'numeric'
fix.numeric <- function(DF){ DF %>% 
    mutate_at(vars(2:12), str_remove_all, pattern = ",") %>% 
    mutate_at(vars(2:12), str_replace_all, pattern = "^\\s*-\\s*$", "0") %>%
    mutate_at(vars(2:12), as.numeric) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
}

# Separate Commodity ID from Description
split.id.desc <- function(DF, com_col = 1){ DF %>% 
    rename(com_orig = com_col) %>% 
    mutate(com_id = str_extract(com_orig, "^[0-9]+")) %>% 
    select(com_id, everything())
}

# Removing rows of NA, they aren't needed
delete.na <- function(DF) {
  # If id's have anything but digits, it will need to be removed first
  DF$com_id <- gsub("'|,", '', DF$com_id)
  DF$com_id <- str_trim(DF$com_id, "both")
  # To remove any row that doesn't start with specific digits
  DF <- DF[str_detect(DF$com_id, pattern = "^(0|1|2|3|4|7|8|9)\\d*$"), ]
  # To remove any row that has NA in com_id
  DF <- DF[!is.na(DF$com_id), ]
  DF
}

# Add leading zero where they're missing
pad.left <- function(DF, char = '0', stop_row = '10'){ DF %>% 
    # Create new group_id when you find com_id = 1
    mutate(grp = cumsum(str_detect(com_id, "^1$|^01$"))) %>% 
    group_by(grp) %>% 
    # Mutate if the ID is missing the leading 0
    mutate(com_id = ifelse(!str_detect(com_id, '^0') & 
                             row_number() <= which(lead(com_id == stop_row)),
                           str_pad(com_id, str_length(com_id) + 1, 'left', char), 
                           com_id)) %>% 
    ungroup() %>% select(-grp)
}

# Load and prepare AAR Commodity Codes & Descriptions
load.aar <- function(AAR){ 
  AAR <- read.csv(file = AAR_dataPath,
                  colClasses = "character")
  AAR_Com_Code <- AAR[, c(1, 8)]
  AAR_Com_Code <- as_tibble(lapply(AAR_Com_Code, trimws), stringsAsFactors = FALSE)
  AAR_Com_Code <- AAR_Com_Code[!apply(AAR_Com_Code == "", 1, all), ]
  AAR_Com_Code <- AAR_Com_Code[str_detect(AAR_Com_Code$STCC, pattern = "\\d+"), ]
  AAR_Com_Code
}

# Add Descriptions for official com_id values, NA for unofficial ones
add.desc <- function(DF){ DF %>%
    left_join(AAR_Com_Code %>% select(STCC, Traditional.FCS.Name), by = c('com_id' = 'STCC')) %>%
    select(com_id, com_desc = Traditional.FCS.Name, everything())
}

## ------------- 3 - Import & Clean ---------------------------
# Turn all Excel worksheets into CSV files 
xl_files <- list.files(raw_dataPath, pattern = "\\.xlsx?", full.names = T)
BNSF_xl <- NULL # Initialize BNSF_xl since map_df throws an error 
for (i in 1:length(xl_files)) {
  xl_sheets <- xl_files[i] %>% # Character vector of sheet names
    excel_sheets() %>% 
    set_names()
  BNSF_xl <- map_df(
    xl_sheets,
    ~ read_excel(xl_files[i], sheet = .x, col_names = F),
  ) %>% 
    write_csv(path = paste0(raw_dataPath, "CSV-", str_extract(xl_files[i], "BNSF_\\d{4}_(\\d{1}|\\w{6})"), ".csv"))
}

# Load all BNSF rail data into one dataframe for mass clean up
BNSF_all <- list.files(path = raw_dataPath, pattern = ".*.csv", full.names = T) %>% #will need to be more explicit
  lapply(read.csv, header = F, stringsAsFactors = F, na.strings=c("","\\s+","NA")) %>% 
  bind_rows

# Assigning a temp variable so when I ruin the table I can just run this line and try again
# Making it a tibble to be easier to work with
tempBNSF <- as_tibble(BNSF_all)

# Checking some details to know where I start
str(tempBNSF)
colSums(is.na(tempBNSF))

# Giving temp column names for easy reference, and dropping columns full of NAs
tempBNSF <- select(tempBNSF, com_id = 1, y = 2:12)

# Clearing commas and changing class to numeric for all observations
tempBNSF <- fix.numeric(tempBNSF)

# Separating commodity id from commodity description
tempBNSF <- split.id.desc(tempBNSF)
tempBNSF <- select(tempBNSF, -com_orig)

# View(tempBNSF %>% filter(str_detect(com_id, "^0?1$"))) # Are there 32 periods? Yes! (also, no whitespace or other crap)
sum(na.omit(str_detect(tempBNSF$com_id, "^0?1$"))) # Since every period starts with "01" or "1" (and it's unique), this should = 32
sum(na.omit(str_detect(tempBNSF$com_id, "^0?12$"))) # trying with different ID, to double check

# Removing rows of NA, they aren't needed and they're in the way!
tempBNSF <- delete.na(tempBNSF)

# This is the last entry for each period, I can use this to determine total rows per time period
which(tempBNSF$com_id == '48')
sort(diff(which(tempBNSF$com_id == '48'))) # There are 470 rows per time period
View(filter(tempBNSF, com_id == "10")) # This is the first ID that doesn't need padding and it's present in all periods, thankfully.
View(filter(tempBNSF, lead(com_id == "10"))) # This is the last ID that needs padding 

# Make sure there's a leading zero for (up to) first 65 commodity codes (per time period)
tempBNSF <- pad.left(tempBNSF)

# Importing (and cleaning) AAR Code List
AAR_Com_Code <- load.aar()

tempBNSF <- add.desc(tempBNSF)


## ------------- 4 - Verification -------------------
# Adds a column to verify codes match up with AAR Commodity Codes
tempBNSF <- mutate(tempBNSF, 
                   com_verify = tempBNSF$com_id %in% AAR_Com_Code$STCC)

# If I zero padded the wrong code or missed padding one that needed it, we'd see it here
View(filter(tempBNSF, tempBNSF$com_verify == F))
colSums(is.na(tempBNSF))


###### 5 - Export ########
# Dataframe needs clarification
quarter <- c("Q1", "Q2", "Q3", "Q4", "Q0")
yr <- as.character(c('2013':'2019'))    #seq.Date(2013/1/1, 2019/1/1, years)#years(2013:2019)
tempBNSF$quarter <- 0

tempBNSF %<>%  mutate(grp = cumsum(str_detect(com_id, "^1$|^01$"))) %>% 
  group_by(grp) #%>% mutate(year = 
for (i in 1:nrow(tempBNSF)) { 
  n <- tempBNSF$grp[i] %% 5
  if(n == 0) {n <- 5}
  tempBNSF$quarter[i] <- quarter[n]
}

tempBNSF %<>%
  mutate(
    year = case_when(
      grp < 6   ~ yr[1],
      grp < 11  ~ yr[2],
      grp < 16  ~ yr[3],
      grp < 21  ~ yr[4],
      grp < 26  ~ yr[5],
      grp < 31  ~ yr[6],
      TRUE      ~ yr[7]
    )
  ) %>% 
  ungroup(grp) %>% select(-grp)


# Name columns more appropriately
# Waited till just before export because some column names are long and make it harder to look at data
colnames(tempBNSF) <- c("commodity_id",
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
                        "commodity_verify",
                        "quarter",
                        "year")

tempBNSF %<>% select(year, quarter, everything())

write_csv(tempBNSF,
          paste0(clean_dataPath, "BNSF_2013-2019q2.csv"))
