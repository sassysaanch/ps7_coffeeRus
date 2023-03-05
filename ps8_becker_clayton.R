################################################################################
##
## [ PROJ ] < Problem set 8 >
## [ FILE ] < ps8_becker_clayton.R >
## [ AUTH ] < cnbecker14 >
## [ INIT ] < March 4th, 2023 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------

library(tidyverse)

## ---------------------------
## directory paths
## ---------------------------

plots_dir <- file.path(".", "plots/")
data_dir <- file.path(".", "data/")
csv_dir <- file.path(".", "data/csv_data")
dict_dir <- file.path(".", "data/dictionaries")
stata_dir <- file.path(".", "data/stata_files")

## -----------------------------------------------------------------------------
## Part 1 - Label each question using comments
## -----------------------------------------------------------------------------

# Question 3

download_file <- function(files, file_dirs, suffixes , extensions) {
  data_url <- str_c('https://nces.ed.gov/ipeds/datacenter/data/', files, suffixes,".zip")
  data_zipfile <- str_c(file_dirs, files, ".zip")
  data_unzipped <- str_c(file_dirs, files, extensions)
  if(file.exists(data_zipfile) == FALSE) {
    writeLines(str_c("Dowloading file: ", data_zipfile, " & Unzipping file: ", data_unzipped))
    download.file(url = data_url, destfile = data_zipfile)
    unzip(data_zipfile, exdir = file_dirs)
  } else if (file.exists(data_unzipped) == FALSE) {
    writeLines(str_c("Unzipping file: ", data_unzipped))
    unzip(data_zipfile, exdir = file_dirs)
  } else {
    writeLines(str_c("Already have files: ", data_zipfile, " ", data_unzipped))
  }
}

# Question 4 

# I've taken 2019 and 2018 as year 1 and year 2

# Question 5

download_file(files = "ef2019a", file_dirs = file_dirs[1], suffixes = suffixes[1], extensions = extensions[1])
download_file(files = "ef2019a", file_dirs = file_dirs[2], suffixes = suffixes[2], extensions = extensions[2])

# Question 6
    
download_data <- function(files, file_dirs, suffixes = "" , extensions = ".csv") {
  data_url <- str_c('https://nces.ed.gov/ipeds/datacenter/data/', files, suffixes,".zip")
  data_zipfile <- str_c(file_dirs, files, ".zip")
  data_unzipped <- str_c(file_dirs, files, extensions)
  if(file.exists(data_zipfile) == FALSE) {
    writeLines(str_c("Dowloading file: ", data_zipfile, " & Unzipping file: ", data_unzipped))
    download.file(url = data_url, destfile = data_zipfile)
    unzip(data_zipfile, exdir = file_dirs)
  } else if (file.exists(data_unzipped) == FALSE) {
    writeLines(str_c("Unzipping file: ", data_unzipped))
    unzip(data_zipfile, exdir = file_dirs)
  } else {
    writeLines(str_c("Already have files: ", data_zipfile, " ", data_unzipped))
  }
}

# Question 7

download_file(files = "ef2018a", file_dirs = file_dirs[1]) # correct, does not work
download_data(files = "ef2018a", file_dirs = file_dirs[1]) # oui, ça marche


## -----------------------------------------------------------------------------
## Part 2 - Label each question using comments
## -----------------------------------------------------------------------------

# Question 1

hd_full_df <- read_csv(file = file.path(csv_dir, "hd2018.csv"))
names(hd_full_df) <- tolower(names(hd_full_df))

ef_full_df <- read_csv(file = file.path(csv_dir, "ef2019a_rv.csv"))
names(ef_full_df) <- tolower(names(ef_full_df))

# Question 2 

hd_df <- hd_full_df %>% select(unitid, stabbr)

# Question 3 

ef_df <- ef_full_df %>% filter(line == 1) %>% 
  select(unitid, eftotlt, efwhitt, efbkaat, efhispt, 
         efasiat, efaiant, efnhpit, 
         ef2mort, efunknt, efnralt)

# Question 4 

ef_df <- ef_df %>% mutate(
  pct_white = efwhitt/eftotlt,
  pct_black = efbkaat/eftotlt,
  pct_latinx = efhispt/eftotlt,
  pct_asian = efasiat/eftotlt,
  pct_amerindian = efaiant/eftotlt,
  pct_nativehawaii = efnhpit/eftotlt,
  pct_tworaces = ef2mort/eftotlt,
  pct_unknownrace = efunknt/eftotlt,
  pct_nonres = efnralt/eftotlt
) %>% select(unitid, pct_white, pct_black, pct_latinx, 
             pct_asian, pct_amerindian, pct_nativehawaii,
             pct_tworaces, pct_unknownrace, pct_nonres)

# Question 5

merged_df <- inner_join(hd_df, ef_df, by = "unitid") %>% select(-unitid)

# Question 6
        
race_by_state <- merged_df %>% group_by(stabbr) %>% 
  arrange(stabbr) %>% 
  mutate(
  avg_pct_white = mean(pct_white, na.rm = T),
  avg_pct_black = mean(pct_black, na.rm = T),
  avg_pct_latinx = mean(pct_latinx, na.rm = T),
  avg_pct_asian = mean(pct_asian, na.rm = T),
  avg_pct_amerindian = mean(pct_amerindian, na.rm = T),
  avg_pct_nativehawaii = mean(pct_nativehawaii, na.rm = T), 
  avg_tworaces = mean(pct_tworaces, na.rm = T), 
  avg_unknownrace = mean(pct_unknownrace, na.rm = T),
  avg_pct_nonres = mean(pct_nonres, na.rm = T)
) %>% filter(row_number() == 1) %>% select(1,11:19)

head(race_by_state, n = 10)


## -----------------------------------------------------------------------------
## Part 3 - Label each question using comments
## -----------------------------------------------------------------------------

# Question 1

read_csv_to_df <- function(dir_name, file_name) {
  year_2_full_df <- read_csv(file = file.path(str_c(dir_name, file_name, ".csv")))
  names(year_2_full_df) <- tolower(names(year_2_full_df))
  year_2_full_df
}

# Question 2

hd_full_df2 <- read_csv_to_df(dir_name = csv_dir, file_name = "hd2018")
ef_full_df2 <- read_csv_to_df(dir_name = csv_dir, file_name = "ef2018a_rv")

# Question 3

create_race_table <- function(dir_name, data_year) {
  read_csv_to_df <- function(dir_name, file_name) {
    year_2_full_df <- read_csv(file = file.path(str_c(dir_name, file_name, ".csv")))
    names(year_2_full_df) <- tolower(names(year_2_full_df))
    year_2_full_df}
  hd_full_df2 <- read_csv_to_df(dir_name = csv_dir, file_name = str_c("hd", data_year))
  ef_full_df2 <- read_csv_to_df(dir_name = csv_dir, file_name = str_c("ef", data_year, "a_rv"))
  hd_df2 <- hd_full_df2 %>% select(unitid, stabbr)
  ef_df2 <- ef_full_df2 %>% filter(line == 1) %>% 
    select(unitid, eftotlt, efwhitt, efbkaat, efhispt, 
           efasiat, efaiant, efnhpit, 
           ef2mort, efunknt, efnralt)
  ef_df2 <- ef_df2 %>% mutate(
    pct_white = efwhitt/eftotlt,
    pct_black = efbkaat/eftotlt,
    pct_latinx = efhispt/eftotlt,
    pct_asian = efasiat/eftotlt,
    pct_amerindian = efaiant/eftotlt,
    pct_nativehawaii = efnhpit/eftotlt,
    pct_tworaces = ef2mort/eftotlt,
    pct_unknownrace = efunknt/eftotlt,
    pct_nonres = efnralt/eftotlt
  ) %>% select(unitid, pct_white, pct_black, pct_latinx, 
               pct_asian, pct_amerindian, pct_nativehawaii,
               pct_tworaces, pct_unknownrace, pct_nonres)
  merged_df2 <- inner_join(hd_df2, ef_df2, by = "unitid") %>% select(-unitid)
  race_by_state2 <- merged_df2 %>% group_by(stabbr) %>% 
    arrange(stabbr) %>% 
    mutate(
      avg_pct_white = mean(pct_white, na.rm = T),
      avg_pct_black = mean(pct_black, na.rm = T),
      avg_pct_latinx = mean(pct_latinx, na.rm = T),
      avg_pct_asian = mean(pct_asian, na.rm = T),
      avg_pct_amerindian = mean(pct_amerindian, na.rm = T),
      avg_pct_nativehawaii = mean(pct_nativehawaii, na.rm = T), 
      avg_tworaces = mean(pct_tworaces, na.rm = T), 
      avg_unknownrace = mean(pct_unknownrace, na.rm = T),
      avg_pct_nonres = mean(pct_nonres, na.rm = T)
    ) %>% filter(row_number() == 1) %>% select(1,11:19)
  race_by_state2
}

create_race_table <- function(dir_name, data_year) {
  hd <- read_csv(file = file.path(str_c(dir_name, str_c("hd", data_year, ".csv")))) %>% 
    select(UNITID, STABBR)
  names(hd) <- tolower(names(hd))
  ef <- read_csv_to_df(dir_name, file_name = str_c("ef", data_year, "a_rv"))
  names(ef) <- tolower(names(ef))
  ef <- ef %>% filter(line ==1) %>% 
    select(unitid, eftotlt, efwhitt, efbkaat, efhispt,
           efasiat, efaiant, efnhpit,
           ef2mort, efunknt, efnralt)
  ef <- ef %>% mutate(
    pct_white = efwhitt/eftotlt,
    pct_black = efbkaat/eftotlt,
    pct_latinx = efhispt/eftotlt,
    pct_asian = efasiat/eftotlt,
    pct_amerindian = efaiant/eftotlt,
    pct_nativehawaii = efnhpit/eftotlt,
    pct_tworaces = ef2mort/eftotlt,
    pct_unknownrace = efunknt/eftotlt,
    pct_nonres = efnralt/eftotlt
  ) %>% select(unitid, pct_white, pct_black, pct_latinx, 
               pct_asian, pct_amerindian, pct_nativehawaii,
               pct_tworaces, pct_unknownrace, pct_nonres)
  merged_df2 <- inner_join(hd, ef, by = "unitid") %>% select(-unitid)
  race_by_state2 <- merged_df2 %>% group_by(stabbr) %>% 
    arrange(stabbr) %>% 
    mutate(
      avg_pct_white = mean(pct_white, na.rm = T),
      avg_pct_black = mean(pct_black, na.rm = T),
      avg_pct_latinx = mean(pct_latinx, na.rm = T),
      avg_pct_asian = mean(pct_asian, na.rm = T),
      avg_pct_amerindian = mean(pct_amerindian, na.rm = T),
      avg_pct_nativehawaii = mean(pct_nativehawaii, na.rm = T), 
      avg_tworaces = mean(pct_tworaces, na.rm = T), 
      avg_unknownrace = mean(pct_unknownrace, na.rm = T),
      avg_pct_nonres = mean(pct_nonres, na.rm = T)
    ) %>% filter(row_number() == 1) %>% select(1,11:19)
  race_by_state2
}

# Question 4

race_by_state2 <- create_race_table(dir_name = csv_dir, data_year = 2018)

head(race_by_state)
head(race_by_state2)


## -----------------------------------------------------------------------------
## Part 4 - Label each question using comments
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
