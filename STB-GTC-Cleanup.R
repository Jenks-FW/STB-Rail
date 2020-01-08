###### Header ###########################################################
## Cleaning up STB GTC Rail data - Brad Jenkins (with help from Brad Hill)
## bjenkins@freightwaves.com    11/11/19


###### 0 - Load Libraries ########
library(tidyverse)


####### 1 - Source files #########
dataPath  <- "C:/Users/bjenkins/Documents/Datasets/Surface Trans Board Data/gtc"
AAR_dataPath <- "C:/Users/bjenkins/Documents/Datasets/STB-Data/AAR-Commodity-Code.csv"
clean_dataPath <- "C:/Users/bjenkins/Documents/Datasets/STB-Data/STB-Clean-Data/"
#dataFile  <-  "some_functions.R" 
#source(paste0(dataPath,dataFile))


####### 2 - Functions #########
# Remove commas, change appropriate columns from 'character' to 'numeric'  
fix.numeric <- function(DF){ DF %>% 
    mutate_at(vars(3:13), str_remove_all, pattern = ",") %>% 
    mutate_at(vars(3:13), as.numeric)
}

# Separate Commodity ID from Description
split.id.desc <- function(DF, com_col = 1){ DF %>% 
    rename(com_orig = com_col) %>% 
    mutate(com_id = str_extract(com_orig, "^[0-9]+"),
           com_desc = str_trim(str_remove(com_orig, com_id))) %>% 
    select(com_id, com_desc, everything())
}

# Combine descriptions into com_desc column
combine.desc <- function(DF){ DF %>%
    mutate(com_desc = na_if(com_desc, ""),
           V2 = na_if(V2, ""),
           com_desc = coalesce(com_desc, V2)) %>% 
    select(-c(3:4))
}

# Removing rows of NA, they aren't needed and they're in the way!
delete.na <- function(DF, n = 6) {
  DF[!is.na(DF$com_id), ]          # Changed to remove all columns w/out commodity ID so "Total" rows would get deleted
  #DF[rowSums(is.na(DF)) <= n, ]   # This leaves "Total" columns in DF
}

# Add leading zero when they're missing
pad.left <- function(DF, char = '0'){ 
  range_vector <- DF$com_desc[1:65]
  DF %>% mutate(com_id = ifelse(com_desc %in% range_vector & !str_detect(com_id, "^0"), 
                                str_pad(com_id, str_length(com_id) + 1, "left", char), com_id))
}

# Load and prepare AAR Commodity Codes & Descriptions
load.aar <- function(AAR){ 
  AAR <- read.csv(file = AAR_dataPath,
                  colClasses = "character")
  AAR_Com_Code <- AAR[, c(1, 8)]
  AAR_Com_Code <- as_tibble(lapply(AAR_Com_Code, trimws), stringsAsFactors = FALSE)
  AAR_Com_Code <- AAR_Com_Code[!apply(AAR_Com_Code == "", 1, all), ]
  AAR_Com_Code <- AAR_Com_Code[-466, ]  # I don't need this row
  AAR_Com_Code
}


###### 3 - Import & Clean the Data ########
# Create data frame from a single csv file to practice manipulating
#GTC_13_1 <- read.csv(file.choose(new = T), header = F, stringsAsFactors = F)

# Load all GTC rail data into one dataframe for convenient mass cleaning
GTC_all <- list.files(path = dataPath, pattern = ".*.csv", full.names = T) %>% 
    lapply(read.csv, header = F, stringsAsFactors = F) %>% 
    bind_rows

# Assigning a temp variable so when I ruin the table I can run this line and try again
# Making it a tibble to be easier to work with the large data frame
#tempGTC <- as_tibble(GTC_13_1) 
tempGTC <- as_tibble(GTC_all)

# Checking the DF details to know where I start
str(tempGTC)
View(tempGTC)
  
# Clearing commas and changing class to numeric for all measured variables
tempGTC <- fix.numeric(tempGTC)

# Separating commodity id from commodity description
tempGTC <- split.id.desc(tempGTC)

# Some descriptions were already in a separate column, some were not
# This combines them and removes the extra column for consistency
tempGTC <- combine.desc(tempGTC)

# Giving simple temp names for easy reference
tempGTC <- select(tempGTC, com_id, com_desc, y = 3:13)

# Removing rows of NA, they aren't needed and they're in the way!
tempGTC <- delete.na(tempGTC)
# Sanity check: there are 32 GTC spreadsheets with 468 rows of data in each. 
# So there should be 14,976 rows total after deleting the extras
# Note: there are 465 Official Commodity Codes. GTC Rail data has 3 unofficial codes

# Make sure there's a leading zero for the first 65 commodity codes (per time period)
tempGTC <- pad.left(tempGTC)


###### 4 - Verification ########
# Importing (and cleaning) AAR Code List to check integrity
# Importing (and cleaning) AAR Code List
AAR_Com_Code <- load.aar()

# Adds a column to verify codes match up with AAR Commodity Codes
# All unofficial commodity codes will show FALSE (should be 3 per time period)
tempGTC <- mutate(tempGTC, com_verify = tempGTC$com_id %in% AAR_Com_Code$STCC)

# If I zero padded the wrong code or missed padding one that needed it, we'd see it here
View(filter(tempGTC, tempGTC$com_verify == F))
sum(is.na(tempGTC$com_desc))


# Confirm there's exactly 96 (3 rows x 32 periods) rows showing FALSE
sum(tempGTC$com_verify == FALSE)


###### 5 - Export Clean Data ########
# Name columns more appropriately
# Waited until just before export because some column names are long and make it harder to look at data
colnames(tempGTC) <- c("com_id",
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

# Save that squeaky clean data to CSV!
write.csv(tempGTC, 
          file = clean_dataPath + "GTC_all.csv", 
          row.names = FALSE)
