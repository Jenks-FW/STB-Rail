


#### Stores file location, vector of excel files and vector of sheet names
dataPath  <- "C:/Users/bjenkins/Documents/Datasets/Surface Trans Board Data/bnsf/"
xl_files <- list.files(dataPath, pattern = "\\.xlsx?", full.names = T)
xl_sheets <- xl_files[1] %>% 
  excel_sheets() %>% 
  set_names()
###################################################################################################

#### WORKS FOR ALL SHEETS IN ONE EXCEL FILE ####################################################
BNSF_xl <- map_df(
  xl_sheets,
  ~ read_excel(xl_files[1], sheet = .x, col_names = F),
  #.id = "sheet"
) %>% 
  write_csv(path = paste0(dataPath, "CSV-", str_extract(xl_files[1], "BNSF_\\d{4}_\\d{1}"), ".csv"))
########################################################################################################

### THIS WORKS AND GETS ALL SHEETS INTO CSVs!!!!!!  ###########################################################
BNSF_xl <- NULL
for (i in 1:length(xl_files)) {
  xl_sheets <- xl_files[i] %>% 
    excel_sheets() %>% 
    set_names()
  #ranges <- list("A5:F15", cell_rows(5:15))
  BNSF_xl <- map_df(
    xl_sheets,
    ~ read_excel(xl_files[i], sheet = .x, col_names = F),
#    .id = "sheet"
  ) %>% 
#    data_list[[i]] <- BNSF_xl
  write_csv(path = paste0(dataPath, "CSV-", str_extract(xl_files[i], "BNSF_\\d{4}_(\\d{1}|\\w{6})"), ".csv"))
  next(i)
}
##################################################################################################################

#### OTHER POSSIBLE APPROACHES ###################################################################################
#Double mapped?
read_all_sheets <- function(xl_file){
  xl_sheets <- set_names(excel_sheets(xl_file))
  map_df(xl_sheets, ~read_excel(xl_file, sheet = .x, col_names = F))
}
map_df(xlFiles, read_all_sheets)

#Nested for loops
for (i in 1:length(xl_files)) {
  xl_sheets <- excel_sheets(xl_files[i]) %>% set_names()
  for (j in 1:length(xl_sheets)){
    BNSF_xl[[i]][[j]] <- read_excel(xl_files[i], sheet = xl_sheets[j], col_names = F)
  }
}
##################################################################################################################

# Convert to CSV
xl_files <- list.files(raw_dataPath, pattern = "\\.xlsx?", full.names = T)
all_rail_xl <- NULL # Initialize all_rail_xl since map_df throws an error 
for (i in 1:length(xl_files)) {
  if (length(excel_sheets(xl_files[i])) > 1) {
   xl_sheets <- xl_files[i] %>% # Character vector of sheet names
     excel_sheets() %>% 
     set_names()
  } else {
  xl_sheets <- excel_sheets(xl_files[i])
  }
  all_rail_xl <- map_df(
    xl_sheets,
    ~ read_excel(xl_files[i], sheet = .x, col_names = F),
  ) %>% 
    write_csv(path = paste0(raw_dataPath, "CSV-", str_extract(xl_files[i], "(BNSF)|(CSX)|(GTC)|(KCS)|(NS)|(SOO)|(UP)"), ".csv"))
}

### GARBAGE ###############################################################################
list_all <- lapply(sheet_names, function(x) read_excel(path = xl_files[1], sheet = x))
BNSF_xl <- map_dfr(
  xl_sheets,
  ~ read_excel(xl_files, sheet = .x, col_names = F),
  .id = "sheet"
) %>% 
  write_csv(BNSF-raw.csv)

BNSF_raw <- bind_rows(data_list)

BNSF_xl <- read_excel(dataPath, sheet = "iris") %>% write_csv("BNSF-raw.csv")

print(deaths, n = Inf)

tab_names <- excel_sheets(path = xl_data)
list_all <- lapply(tab_names, function(x) read_excel(path = xl_data, sheet = x))
#########################################################################################


### ATTTMPTS AT ADDING DESCRIPTIONS #####################################################
#try1
add.desc <- function(DF, com_id = 1){ DF %>% 
    range_vector <- AAR_Com_Code$STCC
    mutate(com_desc = ifelse(com_id %in% range_vector), 
           com_desc <- AAR_Com_Code$Traditional.FCS.Name, NA) %>%
      select(com_id, com_desc, everything())
}


#try2
add.desc <- function(DF){
  range_vector <- AAR_Com_Code$STCC
  DF %>%  
    transmute(com_desc = ifelse(com_id %in% range_vector, 
                                AAR_Com_Code$Traditional.FCS.Name, NA)) %>%
    select(com_id, com_desc, everything())
}


#try3 - WORKS
add.desc <- function(DF){ DF %>% 
  rename(com_desc = 1, com_id = 2) %>%
  left_join(AAR_Com_Code %>% select(STCC, Traditional.FCS.Name), by = c('com_id' = 'STCC')) %>%
  select(com_id, com_desc = Traditional.FCS.Name, everything())
}
#####################################################################################################

### WORKING ON A PAD FUNCTION #######################################################################
#try1
# Add leading zero where they're missing, not robust enough to run on CSX_all yet
pad.left <- function(DF, char = '0'){ DF %>%
    mutate_if(com_id, str_detect(com_id, "^1$"), str_pad(com_id, str_length(com_id) + 1, side = "left", "0"))
    mutate("id" = ifelse(str_detect("id", "^01$"), "id", str_pad("id", str_length("id") + 1, "left", "0")))
  }

#mutate(tempRail, tempRail[1] = ifelse(tempRail[2] == 'FARM PRODUCTS', pad.left(tempRail), tempRail[1]))

#try2 - AAR descriptions don't exactly match Rail descriptions
pad.left <- function(DF, char = '0'){ DF %>% 
    mutate(DF[1] = ifelse(DF[2] %in% AAR_Com_Code[2] & !str_detect(AAR_Com_Code[1], "^0"),
                          str_pad(DF[1], str_length(DF[1]) + 1, "left", "0"), 
                          DF[1]))
}

#try3 - WORKING, but only pads exact matches to range_vector
range_vector <- AAR_Com_Code$Traditional.FCS.Name[1:64]

pad.left <- function(DF, char = '0'){ DF %>% 
    mutate(com_id = ifelse(com_desc %in% range_vector & !str_detect(com_id, "^0"), 
                       str_pad(com_id, str_length(com_id) + 1, "left", char), com_id))
}
view(pad.left(tempRail))

#try4 groups and pads properly but filter causes it to drop all rows that don't need padding
pad.left <- function(DF, char = '0'){ DF %>% 
    # Create new group ID when you find com_id = 1
    mutate(grp = cumsum(str_detect(com_id, "^1$"))) %>% 
    group_by(grp) %>% 
    # Filter group rows to 66
    filter(row_number() <= 66) %>% 
    # Mutate if the ID is missing the leading 0
    mutate(com_id = ifelse(!str_detect(com_id, '^0'), 
                           str_pad(com_id, str_length(com_id) + 1, 'left', char), 
                           com_id))
}

tempRail %>% 
  # Create new group ID when you find com_id = 1
  mutate(grp = cumsum(str_detect(com_id, "^0?1$"))) %>% 
  group_by(grp) %>% 
  # Filter group rows to 2 (or 68 or whatever in your case)
  filter(row_number() <= 66) %>% 
  # Mutate if the ID is missing the leading 0
  mutate(id = ifelse(!str_detect(com_id, '^0'), 
                     str_pad(com_id, str_length(com_id) + 1, 'left', '0'), 
                     com_id))

# attempts at creating a range to pad... this doesn't make any sense
pad_range <- tempRail[str_detect(tempRail$com_id, "^1$"),]:tempRail[str_detect(tempRail$com_id, "^98$"),]

##try5 some dumb loopy shit
pad.left <- function(DF, char = '0'){
  if (str_detect(DF$com_id, "^1$")) {
    repeat { DF %>% 
    str_pad(com_id, str_length(com_id) + 1, 'left', char)
  }
  if (str_detect(DF$com_id, "^10$")){
    break
  }
    }
}


pad.left <- function(DF, char = '0'){
  # Mutate if the ID is missing the leading 0
  mutate_if(DF, 
            cumsum(str_detect(DF$com_id, "^1$")),
            repeat{
              str_pad(DF$com_id, str_length(DF$com_id) + 1, 'left', char)
              if (str_detect(DF$com_id, "^10$")){
                break
              }
            })
}

pad.left <- function(DF, char = '0'){ 
  mutate_if(DF,
            str_detect(DF$com_id, "^1$"), 
            str_pad(DF$com_id, str_length(DF$com_id) + 1, side = "left", char))
}


tempRail %>% 
  left_join(AAR_Com_Code %>% 
              rename(fcs_name = 2) %>% 
              mutate(fcs_name = str_replace_all(fcs_name, ",", ", ")), by = c('name' = "fcs_name"))

###### FINAL PAD FUNCTION FOR GTC ###################################################
pad.left <- function(DF, char = '0'){
  range_vector <- 1:65
  DF %>%
    mutate(com_id = ifelse(str_detect(com_id, "^1$"),
                           str_pad(com_id, str_length(com_id) + 1, "left", char), com_id))
}
####################################################################################








### DON'T BOTHER, MARK THEM WITH 'NA's ###
# Remove unofficial commodity codes  # The three extras (for GTC) are 01129 [4,], 47 [466,], & 471 [467,]
# delete.bad.id <- function(DF, bad_id = c(4)){ 
#   for (i in 0:32) {
#     DF[-(bad_id + (470 * i)),]
#     next
#   }
# }

# Data sample to practice on
DF <- tribble(
  ~com_id, ~com_desc,
  "01", "FARM PRODUCTS",
  "011", "FIELD CROPS",
  "0112", "COTTON,RAW",
  "01131", "BARLEY",
  "01132", "CORN, EXCEPT POPCORN",
  "09131", "SHELLS (OYSTER,CRAB,ETC.)",
  "098", "FISH HATCHERIES, FARMS",
  "107", "TUNGSTEN ORES",
  "108", "CHROMIUM ORES",
  "109", "MISCELLANEOUS METAL ORES",
  "11", "COAL",
  "111", "ANTHRACITE",
  "1", "FARM PRODUCTS",
  "11", "FIELD CROPS",
  "112", "COTTON,RAW",
  "1131", "BARLEY",
  "1132", "CORN, EXCEPT POPCORN",
  "9131", "SHELLS (OYSTER,CRAB,ETC.)",
  "98", "FISH HATCHERIES, FARMS",
  "108", "CHROMIUM ORES",
  "109", "MISCELLANEOUS METAL ORES",
  "11", "COAL",
  "111", "ANTHRACITE")


###### 6 - Scratch Code ########
#tempRail <- (tempRail[rowSums(is.na(tempRail)) != ncol(tempRail),])  # DELETES NA ROWS BUT ONLY FOR FULL ROW OF NA's
#tempRail <- pad.left(tempRail) #
#tempRail <- tempRail[-c(1:11), ] #
#tempRail <- tempRail[-c(471:482), ] # IF TABLE DOESN'T END WITH ROW 470 'TOTAL CARLOAD TRAFFIC:'
#rownames(tempRail) <- NULL     # FIX ROW INDEXING IF ROWS WERE DELETED
#tempRail[469:470, 1] <- NA     # CLEAR USELESS CELL CONTENTS
#lapply(tempRail, str_squish)   # REMOVE EXTRA WHITESPACE
#tempRail[469,2] <- "TOTAL CARLOAD TRAFFIC:" # FIX CELL CONTENTS
#tempRail[470,2] <- "TOTAL CARLOAD & LCL:"





#### CSX_ALL ######
## WTF IS GOING ON WITH THE NUMBER OF ROWS PER TIME PERIOD?!
## 32 spreadsheets x 465 rows = 14,880
## Totals
tempRail <- tempRail[str_detect(tempRail$com_id, pattern = "^[0123489]\\d*$"), ]
sum(is.na(tempRail$com_id))  ## 1,669 NA's in com_id row
18280 - 1669     ## 16,611 rows left if they're removed
16611 - 14880    ## 1,731 rows to remove that aren't NA but shouldn't be there
tempRail$com_id <- gsub("'", '', tempRail$com_id)
sum(na.omit(str_detect(tempRail$com_id, pattern = "^\\d+$"))) ## 14,799 rows have nothing but 1+ digits in them 
14880 - 14799 ## 81 rows unaccounted for ()

## Q1_Y2013
sum(is.na(tempRail$com_id[1:482])) ## 16 NA's
482 - 16 ## 466  ### Row 158 is extra
sum(na.omit(str_detect(tempRail$com_id, "^2047$"))) ## unofficial id:2047 found in 31 of 32 sheets

## Q2_Y2013
sum(is.na(tempRail$com_id[488:1038]))  ## 86 NA's
1038 - 487 - 86  ## 465    ## id:2047 at row:158 is extra    ## Missing id:325 at row:323

## Q3_2013
sum(is.na(tempRail$com_id[1051:1516])) ## 0 NA's
1516 - 1050 ## 466

## Q4_2013
sum(is.na(tempRail$com_id[1521:1986])) ## 0 NA's
1986 - 1520 ## 466

## Annual 2013
sum(is.na(tempRail$com_id[1992:2457])) ## 0 NA's
2457 - 1991 ## 466

## Q1_2014
sum(is.na(tempRail$com_id[2462:2927])) ##
2927 - 2461 ## 466

## Q2_2014
sum(is.na(tempRail$com_id[2932:3397])) ##
3397 - 2931 ## 466



###After delete.na
#Q1_2013 = 1 - 466  (466)
#Q2_2013 = 467 - 931 (464)
#Q3_2013 = 932 - 1397 (465)
#Q4_2013 = 1398 - 1863 (465)
#Annual_2013 = 1864 - 2329 (465)
#Q1_2014 = 2330 -  ()






## Functions have to pass from one line to the next or be assigned, otherwise it will just run the last line of code!!!!

DF <- data.frame(com_id = c(NA, NA, 1, 3, 5, 'Code'),
                 x = c(NA, NA, 5, NA, 6, 'other'),
                 y = c(NA, 3, 6, 4, NA, 'test'))
ex_func <- function(DF, n = 2){
  #DF[!is.na(DF$com_id),]
  DF[str_detect(DF$com_id, "^\\d"), ]
  DF[rowSums(is.na(DF)) <= n, ]
}
ex_func(DF)


