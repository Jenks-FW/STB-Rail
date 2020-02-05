###### Header ###########################################################
## Cleaning up STB KCS Rail data - Brad Jenkins (with help from Brad Hill)
## bjenkins@freightwaves.com    12/4/19


###### 0 - Load Libraries ########
library(tidyverse)
library(readr)
library(magrittr)

####### 1 - Source files #########
dataPath  <- "C:/Users/bjenkins/Documents/Datasets/STB-Data/kcs"
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
  # If ID's have anything but digits, it will need to be removed first
  DF$com_id <- gsub("'|,", '', DF$com_id)
  DF$com_id <- str_trim(DF$com_id, "both")
  # To remove any row that doesn't contain only digits
  DF <- DF[str_detect(DF$com_id, pattern = "^\\d*$"), ]
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
    mutate(com_id = ifelse(!str_detect(com_id, '^0') & row_number() <= 66, 
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
  AAR_Com_Code <- AAR_Com_Code[-466, ]  # I don't need this row
  AAR_Com_Code
}

# Add Descriptions for official com_id values, NA for unofficial ones
add.desc <- function(DF){ DF %>%
    left_join(AAR_Com_Code %>% select(STCC, Traditional.FCS.Name), by = c('com_id' = 'STCC')) %>%
    select(com_id, com_desc = Traditional.FCS.Name, everything())
}


###### 3 - Import & Clean the Data ########
# Load all KCS rail data into one dataframe for clean up
KCS_all <- list.files(path = dataPath, pattern = ".*.csv", full.names = T) %>% 
  lapply(read.csv, header = F, stringsAsFactors = F, na.strings=c(""," ","NA")) %>% 
  bind_rows

# Assigning a temp variable so when I ruin the table I can just run this line and try again
# Making it a tibble to be easier to work with
tempKCS <- as_tibble(KCS_all)

# Checking the DF details to know where I start
str(tempKCS)
colSums(is.na(tempKCS))

# Col 13 is useless so I remove it and rename the others for convenience
tempKCS <- select(tempKCS, com_id = 1, y = 2:12)

# Clearing commas out, replacing '-' with '0', and changing class to numeric for all measured data
tempKCS <- fix.numeric(tempKCS)

# sum(na.omit(str_detect(tempKCS$com_id, "^0")))    # There are no leading zeroes in KCS data
# sum(na.omit(str_detect(tempKCS$com_id, "^\\s*1\\s*$"))) # There seems to be only 31 of 32 spreadsheets?! ... Whitespace is a bitch, everything is there
# sum(na.omit(str_detect(tempKCS$com_id, "STCC"))) # This header appears 32 times, so where's the missing "1"?
# View(tempKCS %>% filter(str_detect(com_id, "STCC") | str_detect(com_id, "^1$"))) # Found the missing "1"

# Removing rows that aren't needed!
tempKCS <- delete.na(tempKCS)
# Sanity check: there should be 32 KCS spreadsheets with 476 rows of data in each.
# So there should be 15,232 rows total after deleting extras
# Note: there are 465 Official Commodity Codes, KCS Rail data has 476
which(tempKCS$com_id == '48')
diff(which(tempKCS$com_id == '48'))
# 32*476    ##15,232
# 952-476   ##476  KCS_13_2
# 1428-952  ##476  KCS_13_3
# 1904-1428 ##476  KCS_13_4
# 2380-1904 ##476  KCS_13 Annual
# 2856-2380 ##476  KCS_14_1
# 3332-2856 ##476  KCS_14_2
# 3571-3332 ##239  KCS_14_3: Here's the problem child, commas in the ID field caused several rows of data to get deleted (fixed delete.na function)
# KCS_14_3 <- read.csv(file.choose(new = T), header = F, stringsAsFactors = F)

# Add a leading zero for the first 66 commodity codes (per time period)
tempKCS <- pad.left(tempKCS)

# Importing (and cleaning) AAR Code List
AAR_Com_Code <- load.aar()

# Add Descriptions from AAR list by matching com_id to AAR Commodity ID's
# Any com_id that isn't on the list gets 'NA' in place of a description
tempKCS <- add.desc(tempKCS)
sum(is.na(tempKCS$com_desc)) # 353 NA's found (ID's that don't match official list)
# (476-465)*32 # There should be 352 NA's for the unnoficial ID's but I still have one more row than expected
# View(filter(tempKCS, com_verify == FALSE)) # Row 13,805 = 2018; we need to drop this row that's clearly a date and not one of the ID's
good_rows <- tempKCS$com_id[1:476]
tempKCS <- filter(tempKCS, com_id %in% good_rows)


###### 4 - Verification ########
# Adds a column to verify codes match up with AAR Commodity Codes
tempKCS <- mutate(tempKCS, com_verify = tempKCS$com_id %in% AAR_Com_Code$STCC)

# If I zero padded the wrong code or missed padding one that needed it, we'd see it here
View(filter(tempKCS, tempKCS$com_verify == F))
#View(filter(tempKCS, com_id %!in% AAR_Com_Code$STCC))


###### 5 - Export Clean Data ########
# Dataframe needs clarification
quarter <- c("Q1", "Q2", "Q3", "Q4", "Q0")
yr <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019")
tempKCS$quarter <- 0

tempKCS %<>%  mutate(grp = cumsum(str_detect(com_id, "^1$|^01$"))) %>% 
  group_by(grp) #%>% mutate(year = 

for (i in 1:nrow(tempKCS)) { 
  n <- tempKCS$grp[i] %% 5
  if(n == 0) {n <- 5}
  tempKCS$quarter[i] <- quarter[n]
}

tempKCS %<>%
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
# Waited till just before export because column names are long and make it harder to look at data
colnames(tempKCS) <- c("commodity_id",
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

tempKCS %<>% select(year, quarter, everything())

write_csv(tempKCS,
          paste0(clean_dataPath, "KCS_2013-2019q2.csv"))
