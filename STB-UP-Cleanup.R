###### Header ###########################################################
## Cleaning up STB UP Rail data - Brad Jenkins (with help from Brad Hill)
## bjenkins@freightwaves.com    12/10/19


###### 0 - Load Libraries ########
library(tidyverse)
library(readr)
library(magrittr)


####### 1 - Source files #########
dataPath  <- "C:/Users/bjenkins/Documents/Datasets/STB-Data/up"
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
# Load all UP rail data into one dataframe for mass clean up
UP_all <- list.files(path = dataPath, pattern = ".*.csv", full.names = T) %>% 
  lapply(read.csv, header = F, stringsAsFactors = F, na.strings=c(""," ","NA")) %>% 
  bind_rows

# Assigning a temp variable so when I ruin the table I can just run this line and try again
# Making it a tibble to be easier to work with
tempUP <- as_tibble(UP_all)

# Checking some details to know where I start
str(tempUP)
colSums(is.na(tempUP)) # This data is a mess! Columns don't line up from one period to another

# Columns aren't matching up due to weird formatting from excel. This moves data to the left columns and NA's to the right
tempUP <-  as_tibble(t(apply(tempUP, 1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
#colSums(is.na(tempUP)) # looking at NA totals again... did I break anything? YES :(

# Giving simple temp names for easy reference, and dropping columns full of NAs
tempUP <- select(tempUP, com_id = 1, y = 2:12)

# What data moved to wrong column? What row is it in?
#which(str_detect(tempUP$com_id, "^\\d+$") & is.na(tempUP$y11)) # look at columns with a numeric id and 'NA' in the last column
# These are all the rows that moved data to the wrong column
#68, 237, 589, 661, 662, 1470, 2001, 2022, 2122, 2461, 2524, 2872, 3035, 3609, 4581, 15288, 15508
bad_rows <- tempUP %>% 
  filter(str_detect(com_id, "^\\d+$") & is.na(tempUP$y11)) %>%
  mutate(rownum = which(str_detect(tempUP$com_id, "^\\d+$") & is.na(tempUP$y11)))

# Fix it
t6 <- bad_rows %>% 
  mutate_at(vars(starts_with('y')), na_if, 0) %>% 
  mutate(y11 = coalesce(y9, y10),
         y9 = NA,
         y10 = NA) %>% 
  mutate_at(vars(starts_with('y')), replace_na, 0) 

# Put it back in tempUP
for(i in 1:nrow(t6)){
  tempUP[t6$rownum[i],] <- t6[i,-ncol(t6)]
}

# Clearing commas out, replacing '-' with '0', and changing class to numeric for all measured data
tempUP <- fix.numeric(tempUP)

#View(tempUP %>% filter(str_detect(com_id, "^\\s*'?0?1\\s*$"))) # Are there 32 periods? Yes!
#sum(na.omit(str_detect(tempUP$com_id, "^\\s*'?0?1\\s*$"))) # Since every period starts with "01" or "1" (and it's unique), this should = 32
#sum(na.omit(str_detect(tempUP$com_id, "^\\s*'?0?12\\s*$"))) # Same reasoning, different id, to double check

# Removing rows of NA, they aren't needed and they're in the way!
tempUP <- delete.na(tempUP)

# This is the last entry for each period, I can use this to determine total rows per time period
# which(tempUP$com_id == '48')
# diff(which(tempUP$com_id == '48')) # 467 rows in each UP rail data time period except the first one (435 rows)
# View(filter(tempUP, com_id == "10")) # This is the first ID that doesn't need padding and it's present in all periods, thankfully.
# View(filter(tempUP, lead(com_id == "10")))

# Make sure there's a leading zero for the first commodity codes up to id == 10 (per time period)
tempUP$com_id <- str_trim(tempUP$com_id)
tempUP <- pad.left(tempUP)

# Importing (and cleaning) AAR Code List
AAR_Com_Code <- load.aar()

# Add Descriptions from AAR list by matching com_id to AAR Commodity ID's
# Any com_id that isn't on the list gets 'NA' in place of a description
tempUP <- add.desc(tempUP)


###### 4 - Verification ########
# Adds a column to verify codes match up with AAR Commodity Codes
tempUP <- mutate(tempUP, com_verify = tempUP$com_id %in% AAR_Com_Code$STCC)

# If I zero padded the wrong code or missed padding one that needed it, we'd see it here
View(filter(tempUP, tempUP$com_verify == F))

sum(is.na(tempUP$com_desc)) # 96 NA's found (ID's that don't match official list)
# Since every period had 3 unofficial codes (01129, 47, 471), this is expected


###### 5 - Export Clean Data ########
# Dataframe needs clarification
quarter <- c("Q1", "Q2", "Q3", "Q4", "Q0")
yr <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019")
tempUP$quarter <- 0

tempUP %<>%  mutate(grp = cumsum(str_detect(com_id, "^1$|^01$"))) %>% 
  group_by(grp) #%>% mutate(year = 

for (i in 1:nrow(tempUP)) { 
  n <- tempUP$grp[i] %% 5
  if(n == 0) {n <- 5}
  tempUP$quarter[i] <- quarter[n]
}

tempUP %<>%
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
colnames(tempUP) <- c("commodity_id",
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

tempUP %<>% select(year, quarter, everything())

write_csv(tempUP,
          paste0(clean_dataPath, "UP_2013-2019q2.csv"))
