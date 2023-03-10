################################################################################
##
## [ PROJ ] < Problem set #8 >
## [ FILE ] < Homework Edu 260B >
## [ AUTH ] < Saanchi Shah/ sassysaanch >
## [ INIT ] < 3/5/2023 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------

library(tidyverse)
library(stringr)
library(kableExtra)
library(janitor)

## ---------------------------
## directory paths
## ---------------------------
plots_dir <- file.path("./plots")
data_dir <- file.path("./data")
csv_dir <- file.path("./data/csv_data")
dict_dir <- file.path("./data/dictionaries")
stata_dir <- file.path("./data/stata_files")


## -----------------------------------------------------------------------------
## Part 1 - Label each question using comments
## -----------------------------------------------------------------------------
# Question 3

download_file <- function(dir_name, file_name, file_suffix, file_extension) {
  # function body
  data_url <- str_c("https://nces.ed.gov/ipeds/datacenter/data/", file_name, file_suffix, ".zip")
  
  data_zipfile <- str_c(dir_name, file_name, file_suffix, ".zip")
  
  data_unzipped <- str_c(dir_name, file_name, file_extension)
  
  if(file.exists(data_zipfile) == FALSE || file.exists(data_unzipped) == FALSE){
    download.file(data_url, destfile = data_zipfile)
    unzip(zipfile = data_zipfile,
          exdir = dir_name)
    writeLines(str_c("Downloading file and unzipping file : ", data_unzipped))
  } else if (file.exists(data_unzipped) == TRUE){
    writeLines(str_c("Already have file: ", data_zipfile, data_unzipped))
  }
}


# Question 4, I'll be taking years 2014 and 2015

download_file(dir_name = csv_dir, file_name = 'hd2014', file_suffix = '', file_extension = '.csv')
download_file(dir_name = csv_dir, file_name = 'hd2015', file_suffix = '', file_extension = '.csv')


# Question 5 EF files
download_file(dir_name = csv_dir, file_name = 'ef2014a', file_suffix = '', file_extension = '.csv')
# download_file(dir_name = csv_dir, file_name = 'ef2015a', file_suffix = '', file_extension = '.csv')


# Question 6


download_data <- function(dir_name, file_name, file_suffix = '', file_extension = '.csv') {
  # function body
  data_url <- str_c("https://nces.ed.gov/ipeds/datacenter/data/", file_name, file_suffix, ".zip")
  
  data_zipfile <- str_c(dir_name, file_name, file_suffix, file_extension) 
  
  data_unzipped <- str_c(dir_name, file_name, file_extension)
  
  if(file.exists(data_zipfile) == FALSE || file.exists(data_unzipped) == FALSE){
    download.file(data_url, destfile = data_zipfile)
    unzip(zipfile = data_zipfile,
          exdir = dir_name)
    writeLines(str_c("Downloading file and unzipping file : ", data_unzipped))
  } else if (file.exists(data_unzipped) == TRUE){
    writeLines(str_c("Already have file: ", data_zipfile, data_unzipped))
  }
}


# Question 7 use both download_data and download_file

download_data(dir_name = csv_dir, file_name = 'ef2015a') # this worked
download_file(dir_name = csv_dir, file_name = 'ef2015a') # this did not work


## -----------------------------------------------------------------------------
## Part 2 - Label each question using comments
## -----------------------------------------------------------------------------

# Question 1--------------------------------------------------------------------

# Save HD data to object called `hd_full_df`. Then, change column names to all lowercase.

hd_full_df <- read_csv(file = file.path(csv_dir, "hd2014.csv")) 

# changing column names to lowercase

tolower(names(hd_full_df)) -> names(hd_full_df)

# Save EF data to object called `ef_full_df`. 
ef_full_df <- read_csv(file = file.path(csv_dir, "ef2014a_rv.csv")) 

tolower(names(ef_full_df)) -> names(ef_full_df)

# Question 2--------------------------------------------------------------------

hd_df <- hd_full_df %>% 
  select(unitid, stabbr)

# Question 3--------------------------------------------------------------------

ef_df <- ef_full_df %>% 
  filter(line == 1) %>% 
  select(unitid, 
         eftotlt, 
         efwhitt, 
         efbkaat, 
         efhispt, 
         efasiat, 
         efaiant, 
         efnhpit, 
         ef2mort, 
         efunknt, 
         efnralt)

# Question 4 -------------------------------------------------------------------


ef_df %>% 
  mutate(pct_white = (efwhitt/eftotlt)*100,
         pct_black = (efbkaat/eftotlt)*100,
         pct_latinx = (efhispt/eftotlt)*100,
         pct_asian = (efasiat/eftotlt)*100,
         pct_amerindian = (efaiant/eftotlt)*100,
         pct_nativehawaii = (efnhpit/eftotlt)*100,
         pct_tworaces = (ef2mort/eftotlt)*100,
         pct_unkwownrace = (efunknt/eftotlt)*100,
         pct_nonres = (efnralt/eftotlt)*100
         ) %>% 
  select(unitid, 
         pct_white, 
         pct_black, 
         pct_latinx, 
         pct_asian, 
         pct_amerindian, 
         pct_nativehawaii,
         pct_tworaces,
         pct_unkwownrace,
         pct_nonres
         ) -> ef_df

# Question 5--------------------------------------------------------------------

# join hd and ef
merged_df <- inner_join(hd_df, ef_df, by = "unitid") %>% 
  select(-unitid)

# Question 6--------------------------------------------------------------------

race_by_state <- 
  merged_df %>% 
  group_by(stabbr) %>% 
  mutate(avg_pct_white = mean(pct_white, na.rm = TRUE),
         avg_pct_black = mean(pct_black, na.rm = TRUE),
         avg_pct_latinx = mean(pct_latinx, na.rm = TRUE),
         avg_pct_asian = mean(pct_asian, na.rm = TRUE),
         avg_pct_amerindian = mean(pct_amerindian, na.rm = TRUE),
         avg_pct_nativehawaii = mean(pct_nativehawaii, na.rm = TRUE),
         avg_pct_tworaces = mean(pct_tworaces, na.rm = TRUE),
         avg_pct_unknownrace = mean(pct_unkwownrace, na.rm = TRUE),
         avg_pct_nonres = mean(pct_nonres, na.rm = TRUE)
  )

  # creating a table
  
kable(head(race_by_state, 5), booktabs = TRUE) %>%
  kable_styling(font_size = 8)




## -----------------------------------------------------------------------------
## Part 3 - Label each question using comments
## -----------------------------------------------------------------------------


# Q1

read_csv_to_df <- function(dir_name, file_name) {
 hd_year2 <- read_csv(file.path(csv_dir, str_c(file_name, ".csv"))) %>% 
   clean_names()
}

# used janitor pkg to achieve lowercase

# Question 2

hd_full_df2 <- read_csv_to_df(csv_dir, "hd2015")
ef_full_df2 <- read_csv_to_df(csv_dir, "ef2015a_rv")


# Question 3
create_race_table <- function(dir_name, data_year){
  hd <- read_csv_to_df(dir_name, str_c("hd", data_year)) %>% 
    select(unitid, stabbr) 
  names(hd) <- tolower(names(hd))
  ef <- read_csv_to_df(dir_name, str_c("ef", data_year, "a_rv")) 
  names(ef) <- tolower(names(ef))
  ef %>% 
    filter(line == 1) %>% 
    select(unitid, 
           eftotlt, 
           efwhitt, 
           efbkaat, 
           efhispt, 
           efasiat, 
           efaiant, 
           efnhpit, 
           ef2mort, 
           efunknt, 
           efnralt) %>% 
    mutate(pct_white = (efwhitt/eftotlt)*100,
           pct_black = (efbkaat/eftotlt)*100,
           pct_latinx = (efhispt/eftotlt)*100,
           pct_asian = (efasiat/eftotlt)*100,
           pct_amerindian = (efaiant/eftotlt)*100,
           pct_nativehawaii = (efnhpit/eftotlt)*100,
           pct_tworaces = (ef2mort/eftotlt)*100,
           pct_unkwownrace = (efunknt/eftotlt)*100,
           pct_nonres = (efnralt/eftotlt)*100
    ) %>% 
    select(unitid, 
           pct_white, 
           pct_black, 
           pct_latinx, 
           pct_asian, 
           pct_amerindian, 
           pct_nativehawaii,
           pct_tworaces,
           pct_unkwownrace,
           pct_nonres
    ) -> ef
  
  merged_df2 <- inner_join(hd, ef, by = "unitid") %>% 
  select(-unitid)
  
  merged_df2 %>% 
    group_by(stabbr) %>% 
    mutate(avg_pct_white = mean(pct_white, na.rm = TRUE),
           avg_pct_black = mean(pct_black, na.rm = TRUE),
           avg_pct_latinx = mean(pct_latinx, na.rm = TRUE),
           avg_pct_asian = mean(pct_asian, na.rm = TRUE),
           avg_pct_amerindian = mean(pct_amerindian, na.rm = TRUE),
           avg_pct_nativehawaii = mean(pct_nativehawaii, na.rm = TRUE),
           avg_pct_tworaces = mean(pct_tworaces, na.rm = TRUE),
           avg_pct_unknownrace = mean(pct_unkwownrace, na.rm = TRUE),
           avg_pct_nonres = mean(pct_nonres, na.rm = TRUE))
}
  
race_by_state2 <- create_race_table(csv_dir, 2015)



## -----------------------------------------------------------------------------
## Part 4 - Label each question using comments
## -----------------------------------------------------------------------------


# Question 1-------------------------------------------------------------------

race_state_wb <- race_by_state2 %>%  # could use race_by_state
  select(stabbr, avg_pct_white, avg_pct_black)

race_by_state_wpoc <- race_by_state2 %>% 
  select(stabbr, avg_pct_white, avg_pct_black, avg_pct_amerindian, avg_pct_latinx)

# Question 2-------------------------------------------------------------------

select_race_vars <- function(df, ...) {
  select(df, stabbr, ...)
}


  
# Question 3--------------------------------------------------------------------
  
# use race by state 2

# select variables without using pipes
select_race_vars(race_by_state2, avg_pct_white, avg_pct_black) -> race_by_state2_wb

# select variables using pipes
race_by_state2_wpoc <- race_by_state2 %>% 
  select_race_vars(avg_pct_white, 
                   avg_pct_black, 
                   avg_pct_amerindian,
                   avg_pct_latinx)


# Question 4 -------------------------------------------------------------------
  
# create a pivot table

race_by_state_pivot <- race_by_state_wpoc %>% 
  pivot_longer(
    cols = c(avg_pct_white,
             avg_pct_black,
             avg_pct_amerindian,
             avg_pct_latinx),
    names_to = 'race',
    names_prefix = 'avg_pct_',
    values_to = 'percentage'
  )

# Question 5 ------------------------------------------------------------------

  png(file.path(plots_dir, 'plot2015.png'))
  print(ggplot(race_by_state_pivot, aes(x=race, y=percentage, color=race)) +  
          geom_jitter(width=0.2))
  dev.off()

# Question 6 -------------------------------------------------------------------
  
  
 plot_race_figure <- function(df, dir_name, plot_name) {
   pivot_race <- pivot_longer(df,
                cols = -stabbr,
                names_to = 'race',
                values_to = 'percentage',
                names_prefix = 'avg_pct_')
   png(file.path(dir_name, str_c("plot", plot_name, ".png")))
   print(ggplot(pivot_race, aes(x = race, y = percentage, color = race)) +  
           geom_jitter(width = 0.2))
   dev.off()
 }

# Question 7
  
  plot_race_figure(race_by_state2_wpoc, plots_dir, 2014)
  
# Question 8
  
  create_race_table(csv_dir, 2015) %>% 
    select_race_vars(avg_pct_black, avg_pct_white, avg_pct_latinx, avg_pct_nativehawaii, avg_pct_asian, avg_pct_amerindian) %>% 
    plot_race_figure(plots_dir, "2015")
  
  setwd("/Users/saanchishah/Desktop/rclass2/week7/shah_saanchi_ps7/ps7_coffeeRus")
  
# file_suffix = '', file_extension = '.csv'
## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
  
