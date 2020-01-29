###### Header ###########################################################
## Cleaning up STB CSX Rail data - Brad Jenkins (with help from Brad Hill)
## bjenkins@freightwaves.com    11/18/19


###### 0 - Load Libraries ########
library(tidyverse)
library(readr)


####### 1 - Source files #########
dataPath  <- "C:/Users/bjenkins/Documents/Datasets/STB-Data/csx"
AAR_dataPath <- "C:/Users/bjenkins/Documents/Datasets/STB-Data/AAR-Commodity-Code.csv"
clean_dataPath <- "C:/Users/bjenkins/Documents/Datasets/STB-Data/STB-Clean-Data/"
#dataFile  <-  "some_functions.R" 
#source(paste0(dataPath,dataFile))


####### 2 - Functions ###########
# Remove commas, change appropriate columns from 'character' to 'numeric'
fix.numeric <- function(DF){ DF %>% 
    mutate_at(vars(2:12), str_remove_all, pattern = ",") %>% 
    mutate_at(vars(2:12), str_replace_all, pattern = "^\\s*-\\s*$", "0") %>%
    mutate_at(vars(2:12), as.numeric) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
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
pad.left <- function(DF, char = '0'){ DF %>% 
    # Create new group_id when you find com_id = 1
    mutate(grp = cumsum(str_detect(com_id, "^1$"))) %>% 
    group_by(grp) %>% 
    # Mutate if the ID is missing the leading 0
    mutate(com_id = ifelse(!str_detect(com_id, '^0') & row_number() <= 64, 
                           str_pad(com_id, str_length(com_id) + 1, 'left', char), 
                           com_id)) %>% 
    # Get rid of temp grp, no longer needed
    ungroup() %>% select(-grp)
}

# Load and prepare AAR Commodity Codes & Descriptions
load.aar <- function(AAR){ 
  AAR <- read.csv(file = AAR_dataPath,
                  colClasses = "character")
  AAR_Com_Code <- AAR[, c(1, 8)]
  AAR_Com_Code <- as_tibble(lapply(AAR_Com_Code, trimws), stringsAsFactors = FALSE)
  AAR_Com_Code <- AAR_Com_Code[!apply(AAR_Com_Code == "", 1, all), ]
  AAR_Com_Code <- AAR_Com_Code[-466, ]  # To keep or not to keep?
  AAR_Com_Code
}

# Delete bad Commodity ID, add Description
add.desc <- function(DF){ DF %>%
    rename(com_id = 1) %>%
    left_join(AAR_Com_Code %>% select(STCC, Traditional.FCS.Name), by = c('com_id' = 'STCC')) %>%
    select(com_id, com_desc = Traditional.FCS.Name, everything())
}


###### 3 - Import & Clean the Data ########
# Load all CSX rail data into one dataframe for mass clean up
CSX_all <- list.files(path = dataPath, pattern = ".*.csv", full.names = T) %>% 
    lapply(read.csv, header = F, stringsAsFactors = F, na.strings=c(""," ","NA")) %>% 
    bind_rows

# Assigning a temp variable so when I ruin the table I can just run this line and try again
# Making it a tibble to be easier to work with
tempCSX <- as_tibble(CSX_all)
#tempCSX$com_desc <- NA   # A bug in RStudio Tibble diagnostics throws a million errors for 'uninitialized column' so I initialized it

# Checking the DF details to know where I start
#str(tempCSX)
colSums(is.na(tempCSX))  # Not sure why there's so many columns of NA but we don't need them!
tempCSX <- tempCSX[,-(13:36)]
  
# Giving simple temp names for easy reference, and moving com_desc to col2
tempCSX <- select(tempCSX, com_id = 1, y = 2:12)

# Clearing commas out, replacing '-' with '0', and changing class to numeric for all measured data
tempCSX <- fix.numeric(tempCSX)

#View(tempCSX %>% filter(str_detect(com_id, "^\\s*CODE\\s*$") | str_detect(com_id, "^\\s*'?0?1\\s*$"))) # Are there 32 periods? Yes!
sum(na.omit(str_detect(tempCSX$com_id, "^\\s*'?0?1\\s*$"))) # Since every period starts with "01" or "1" (and it's unique), this should = 32
sum(na.omit(str_detect(tempCSX$com_id, "^\\s*'?0?12\\s*$"))) # Same reasoning, different id, to double check what's lost

# Removing rows of NA, they aren't needed and they're in the way!
tempCSX <- delete.na(tempCSX)

# Sanity check: there should be 25 CSX spreadsheets with 466 rows of data in each.
# So there should be 14,912 rows total after deleting extras          ### 14,932 rows remain 
good_rows <- tempCSX$com_id[1:466]
View(filter(tempCSX, !(com_id %in% good_rows)))   #Several spreadsheets have 469 rows
# Note: there are 465 Official Commodity Codes. Most CSX Rail data has 466

# This com_id has to be an error, all others from CSX have official id 01413 instead of 01433
#View(tempCSX %>% filter(str_detect(com_id, "^0?1413$") | str_detect(com_id, "^0?1433$"))) # 
tempCSX[45,1] <- "01413"

# Add a leading zero to the first 64 commodity codes (per time period)
tempCSX <- pad.left(tempCSX)
sum(str_detect(tempCSX$com_id, "^0")) # =2055 but I expected =2048(64*32);so there's 7 extra codes with leading zero
View(filter(tempCSX, str_detect(tempCSX$com_id, "^0"))) # All periods start with "01" and end with "098", which is good
# zero_pad_range <- tempCSX$com_id[1:64] # These are the codes I expect to have lead zeroes, let's see what's not on this list but has a lead zero
# View(tempCSX %>% 
#        filter(!com_id %in% zero_pad_range & str_detect(tempCSX$com_id, "^0"))) # There are 7 periods with code "01129" 
# which(tempCSX$com_id == "01129", arr.ind=TRUE) # Here's where they're hiding
# zero_pad_range2 <- tempCSX$com_id[11650:11714]

# Everything looks good, time to compare to official AAR Commodity list and add description and verification columns


# Importing (and cleaning) AAR Code List
AAR_Com_Code <- load.aar()

# Add Descriptions from AAR list by matching com_id to AAR
# So any com_id that isn't on the list gets 'NA' instead of a description
tempCSX <- add.desc(tempCSX)
# Since there's 7 periods with 469 rows and 25 with 466 rows, there should be 53 NA's in com_desc ((469-465)*7)+((466-465)*25)
sum(is.na(tempCSX$com_desc)) 


###### 4 - Verification ########
# Adds a column to verify codes match up with AAR Commodity Codes
tempCSX <- mutate(tempCSX, com_verify = tempCSX$com_id %in% AAR_Com_Code$STCC)
# If I zero padded the wrong code or missed padding one that needed it, we'd see it here
View(filter(tempCSX, tempCSX$com_verify == F))


###### 5 - Export Clean Data ########
# Name columns more appropriately
# Waited till just before export because some column names are long and make it harder to look at data
colnames(tempCSX) <- c("com_id",
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

write_csv(tempCSX,
          paste0(clean_dataPath, "CSX_2013-2019q2.csv"))

