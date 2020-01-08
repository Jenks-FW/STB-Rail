###### Header ###########################################################
## Cleaning up STB SOO Rail data - Brad Jenkins (with help from Brad Hill)
## bjenkins@freightwaves.com    12/10/19


###### 0 - Load Libraries ########
library(tidyverse)


####### 1 - Source files #########
dataPath  <- "C:/Users/bjenkins/Documents/Datasets/Surface Trans Board Data/soo"
AAR_dataPath <- "C:/Users/bjenkins/Documents/Datasets/STB-Data/AAR-Commodity-Code.csv"
clean_dataPath <- "C:/Users/bjenkins/Documents/Datasets/STB-Data/STB-Clean-Data/"
#dataFile  <-  "some_functions.R" 
#source(paste0(dataPath,dataFile))


####### 2 - Functions ###########
# Why isn't this already a thing?
`%!in%` <- Negate(`%in%`)

# Remove commas, change appropriate columns from 'character' to 'numeric'
fix.numeric <- function(DF){ DF %>% 
    mutate_at(vars(2:12), str_remove_all, pattern = ",") %>% 
    mutate_at(vars(2:12), str_replace_all, pattern = "-", "0") %>%
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
  AAR_Com_Code <- AAR_Com_Code[-466, ]  # To keep or not to keep?
  AAR_Com_Code
}

# Add Descriptions for official com_id values, NA for unofficial ones
add.desc <- function(DF){ DF %>%
    left_join(AAR_Com_Code %>% select(STCC, Traditional.FCS.Name), by = c('com_id' = 'STCC')) %>%
    select(com_id, com_desc = Traditional.FCS.Name, everything())
}


###### 3 - Import & Clean the Data ########
# Load all SOO rail data into one dataframe for mass clean up
SOO_all <- list.files(path = dataPath, pattern = ".*.csv", full.names = T) %>% 
  lapply(read.csv, header = F, stringsAsFactors = F, na.strings=c(""," ","NA")) %>% 
  bind_rows

# Assigning a temp variable so when I ruin the table I can just run this line and try again
# Making it a tibble to be easier to work with
tempSOO <- as_tibble(SOO_all)

# Checking details to know where I start
str(tempSOO)
colSums(is.na(tempSOO)) # Lots of columns are full of NAs

# It appears SOO added intermediate revenue columns; I don't need those or the NA columns
tempSOO <- select(tempSOO, com_id = 4, y = c(5,6,8,9,11,12,14,15,17,18,19))
#View(filter_all(tempSOO, ~str_detect(.,"[a-zA-Z]"))) # Shows column headers for each period and everything looks correct

# Clearing commas out, replacing '-' with '0', and changing class to numeric for all measured data
tempSOO <- fix.numeric(tempSOO)

#View(tempSOO %>% filter(str_detect(com_id, "^0?1$"))) # Are there 32 periods? Yes! (also, no whitespace or other crap)
#sum(na.omit(str_detect(tempSOO$com_id, "^0?1$"))) # Since every period starts with "01" or "1" (and it's unique), this should = 32
#sum(na.omit(str_detect(tempSOO$com_id, "^0?12$"))) # trying with different ID, to double check

# Removing rows of NA, they aren't needed and they're in the way!
tempSOO <- delete.na(tempSOO)

# This is the last entry for each period, I can use this to determine total rows per time period
# which(tempSOO$com_id == '48')
# sort(diff(which(tempSOO$com_id == '48'))) # There are 382-421 rows per time period
# View(filter(tempSOO, com_id == "10")) # This is the first ID that doesn't need padding and it's present in all periods, thankfully.
# View(filter(tempSOO, lead(com_id == "10")))

# Make sure there's a leading zero for the first commodity codes up to ID: 10 (per time period)
tempSOO <- pad.left(tempSOO)

# Importing (and cleaning) AAR Code List
AAR_Com_Code <- load.aar()

# Add Descriptions from AAR list by matching com_id to AAR Commodity ID's
# Any com_id that isn't on the list gets 'NA' in place of a description
tempSOO <- add.desc(tempSOO)


###### 4 - Verification ########
# Adds a column to verify codes match up with AAR Commodity Codes
tempSOO <- mutate(tempSOO, com_verify = tempSOO$com_id %in% AAR_Com_Code$STCC)

# If I zero padded the wrong code or missed padding one that needed it, we'd see it here
View(filter(tempSOO, tempSOO$com_verify == F))
sum(is.na(tempSOO$com_desc)) # 95 NA's found (ID's that don't match official list)


###### 5 - Export Clean Data ########
# Name columns more appropriately
# Waited till just before export because some column names are long and make it harder to look at data
colnames(tempSOO) <- c("com_id",
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

# Save that squeaky clean data to CSV! Row Names = False so it doesn't start the data set with row indexing
write.csv(tempSOO,
          file = clean_dataPath + "SOO_all.csv", 
          row.names = FALSE)
