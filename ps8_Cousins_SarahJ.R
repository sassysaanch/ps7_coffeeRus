################################################################################
##
## [ PROJ ] < Problem set #8 >
## [ FILE ] < ps8_Cousins_SarahJ >
## [ AUTH ] < Sarah J. Cousins/SJC0usins >
## [ INIT ] < Due 3/10/23 >
##
################################################################################


## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(expss)
library(ggplot2)


## ---------------------------
## directory paths
## ---------------------------
plots_dir <- file.path('.', 'plots')
data_dir <- file.path('.', 'data')
csv_dir <- file.path(data_dir, 'csv_data')
dict_dir <- file.path(data_dir,'dictionaries')
stata_dir <- file.path(data_dir, 'stata_files')

## -----------------------------------------------------------------------------
## Part I: Setting up & downloading data
## -----------------------------------------------------------------------------

#Q1.3 (Revised from PS 7, Q 2.1)
download_file <- function(dir_name, file_name, file_suffix, file_extension){
  data_url <- str_c('https://nces.ed.gov/ipeds/datacenter/data/', file_name, file_suffix,".zip")
  data_zipfile <- str_c(dir_name, str_c(file_name, file_suffix, ".zip"))
  data_unzipped <- str_c(dir_name, str_c(file_name, file_extension))
  if(!file.exists(data_zipfile)) {
    writeLines(str_c("Dowloading file: ", data_zipfile, " & Unzipping file: ", data_unzipped))
    download.file(url = data_url, destfile = data_zipfile)
    unzip(data_zipfile, exdir = dir_name)
  } else if (!file.exists(data_unzipped)) {
    writeLines(str_c("Unzipping file: ", data_unzipped))
    unzip(data_zipfile, exdir = dir_name)
  } else {
    writeLines(str_c("Already have files: ", data_zipfile, " ", data_unzipped))
  }
}


#Q1.4

###Please note that there are only 3 group members. However, years associated wtih
#team member #3 was skipped and thus we downloaded 2019, 2018, 2015, 2014 
#while I downloaded 2013, 2012
#group member #4
## year_1: 2013
## year_2: 2012

download_file(dir_name = csv_dir, file_name = 'hd2012', file_suffix = '', file_extension = '.csv')


#Q1.5 Now, we will start downloading EF data file (e.g., ef2019a). Call download_file() to download:

# CSV data for year_1 (2013) 
######See folder -- HOW TO LIMIT THIS DOWNLOAD TO JUST THE RV.CSV NOT .CSV##########################
download_file(dir_name=file_dirs[1], file_name = "ef2013a", file_suffix = suffixes[1],  extensions[1])
# Data dictionary for year_1 (2013)
download_file(dir_name=file_dirs[2], file_name = "ef2013a", file_suffix = suffixes[2],  extensions[2])


#Q.1.6 copy your download_file() function from Part II, Q6 and change the function name to download_data(). Modify the arguments file_suffix and file_extension to take default values for downloading CSV file types. If you were using the url object (provided in Part II, Q1 of problem set 7) inside your function, make sure to replace it with the string 'https://nces.ed.gov/ipeds/datacenter/data/'.

download_data <- function(dir_name, file_name, file_suffix ="", file_extension = ".csv"){
  data_url <- str_c('https://nces.ed.gov/ipeds/datacenter/data/', file_name, file_suffix,".zip")
  data_zipfile <- str_c(dir_name, str_c(file_name, file_suffix, ".zip"))
  data_unzipped <- str_c(dir_name, str_c(file_name, file_extension))
  if(!file.exists(data_zipfile)) {
    writeLines(str_c("Dowloading file: ", data_zipfile, " & Unzipping file: ", data_unzipped))
    download.file(url = data_url, destfile = data_zipfile)
    unzip(data_zipfile, exdir = dir_name)
  } else if (!file.exists(data_unzipped)) {
    writeLines(str_c("Unzipping file: ", data_unzipped))
    unzip(data_zipfile, exdir = dir_name)
  } else {
    writeLines(str_c("Already have files: ", data_zipfile, " ", data_unzipped))
  }
}

#Q1.7 Now you will try downloading the EF CSV data for year_2 by only providing the dir_name and file_name arguments to the following functions:

#As expected, received an error because  did not provide file_suffix and file_extension
download_file(dir_name=file_dirs[1], file_name = "ef2012a")
#OUTPUT Error in str_c(url, file_name, file_suffix, ".zip"):argument "file_suffix" is missing, with no default

#As expected, downloaded data without file_suffix and file_extension because we have default values for the file_suffix and file_extension arguments
download_data(dir_name=file_dirs[1], file_name = "ef2012a")  


## -----------------------------------------------------------------------------
## Part II: Creating descriptive table outside function
## -----------------------------------------------------------------------------

#Q2.1

hd_full_df <- read_csv(file = file.path(csv_dir, "hd2013.csv"))
names(hd_full_df) <- str_to_lower(names(hd_full_df))

ef_full_df <- read_csv(file = file.path(csv_dir, "ef2013a_rv.csv"))
names(ef_full_df) <- str_to_lower(names(ef_full_df))


#Q2.2 Creating a df w limited vars
hd_df <- hd_full_df %>% select(unitid, stabbr)

#Q2.3 Create a new object ef_df from ef_full_df 

ef_df <- ef_full_df %>% filter(line == 1) %>% #Filter only rows line ==1 ( only full-time, first-time, first-year, degree-seeking undergraduates)
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
         efnralt)#Select limited vars


#Q2.4 

#with eftotlt being the total number of students, created percent vars for each race/ethnicity. 
#divided the count for eac race by the tot number of students and multiplying by 100
ef_df<- ef_df %>% 
  mutate(
  pct_white = (efwhitt/eftotlt)*100,
  pct_black = (efbkaat/eftotlt)*100,
  pct_latinx = (efhispt/eftotlt)*100,
  pct_asian = (efasiat/eftotlt)*100,
  pct_amerindian = (efaiant/eftotlt)*100,
  pct_nativehawaii = (efnhpit/eftotlt)*100,
  pct_tworaces = (ef2mort/eftotlt)*100,
  pct_unknownrace = (efunknt/eftotlt)*100,
  pct_nonres = (efnralt/eftotlt)*100) %>% 
  select(unitid, 
         pct_white, 
         pct_black, 
         pct_latinx, 
         pct_asian, 
         pct_amerindian, 
         pct_nativehawaii,  
         pct_tworaces, 
         pct_unknownrace, 
         pct_nonres) #Selected only the unitid variable and the percent variables 


#Q2.5 
#merge the HD and EF dataframes by the unitid variable. 
#keep rows where unitid exists in both dataframes which informs join strat. 

#Testing the join; Join hd_df and ef_df dfs --> #inner_join(x, y, by = "key") 
merged_df <- inner_join(hd_df, ef_df, by = "unitid") 

#created a new object merged_df and dropped the unitid variable
merged_df <- inner_join(hd_df, ef_df, by = "unitid") %>% 
                       select(-unitid)

#Q2.6
#descriptive table called race_by_state from merged_df with minipulations
#reminder: set na.rm = T
#reminder format: mean(x, trim = 0, na.rm = FALSE, ...) 

race_by_state <- merged_df %>% 
  group_by(stabbr) %>%  #Group by stabbr
    summarize(
    avg_pct_white        = mean(pct_white, na.rm = T),
    avg_pct_black        = mean(pct_black, na.rm = T),
    avg_pct_latinx       = mean(pct_latinx, na.rm = T),
    avg_pct_asian        = mean(pct_asian, na.rm = T),
    avg_pct_amerindian   = mean(pct_amerindian, na.rm = T),
    avg_pct_nativehawaii = mean(pct_nativehawaii, na.rm = T),
    avg_pct_tworaces     = mean(pct_tworaces, na.rm = T),
    avg_pct_unknownrace  = mean(pct_unknownrace, na.rm = T),
    avg_pct_nonres       = mean(pct_nonres, na.rm = T))

## -----------------------------------------------------------------------------
## Part III: Creating descriptive table using functions
## -----------------------------------------------------------------------------

#3.1 First, create the following helper function to read in CSV data and turn the column names to lowercase:Function body:  – Read in the CSV data file from the specified dir_name. The data file would be your file_name input plus the .csv extension. – Turn the column names of the dataframe to all lowercase – Returns: The dataframe from above steps

#Function name: read_csv_to_df() w function argument: dir_name & file_name 
read_csv_to_df <- function(dir_name, file_name){
  df <- read_csv(file = file.path(dir_name, str_c(file_name,".csv")));
  names(df) <- str_to_lower(names(df));
  return(df)
}

#3.2 
#read_csv_to_df() function to read in year_2 for HD and EF files
hd_full_df2 <- read_csv_to_df(csv_dir, "hd2012")
ef_full_df2 <- read_csv_to_df(csv_dir, "ef2012a_rv")

#3.3
create_race_table <- function(dir_name, data_year) {
  hd <- read_csv_to_df(dir_name, str_c("hd", data_year)) %>% 
        select(unitid, stabbr)   
  ef <- read_csv_to_df(dir_name, str_c("ef", data_year, "a_rv")) %>% 
    filter(line ==1) %>% 
    mutate(
      pct_white          = (efwhitt/eftotlt)*100,
      pct_black          = (efbkaat/eftotlt)*100,
      pct_latinx         = (efhispt/eftotlt)*100,
      pct_asian          = (efasiat/eftotlt)*100,
      pct_amerindian     = (efaiant/eftotlt)*100,
      pct_nativehawaii   = (efnhpit/eftotlt)*100,
      pct_tworaces       = (ef2mort/eftotlt)*100,
      pct_unknownrace    = (efunknt/eftotlt)*100,
      pct_nonres         = (efnralt/eftotlt)*100) %>% 
    select(unitid, 
           pct_white, 
           pct_black, 
           pct_latinx, 
           pct_asian, 
           pct_amerindian, 
           pct_nativehawaii,  
           pct_tworaces, 
           pct_unknownrace, 
           pct_nonres) 
  merged_df2 <- inner_join(hd, ef, by = "unitid") %>% 
    select(-unitid) 
  race_by_state <- merged_df2 %>% group_by(stabbr)%>%  #Group by stabbr
    summarize(
      avg_pct_white        = mean(pct_white, na.rm = T),
      avg_pct_black        = mean(pct_black, na.rm = T),
      avg_pct_latinx       = mean(pct_latinx, na.rm = T),
      avg_pct_asian        = mean(pct_asian, na.rm = T),
      avg_pct_amerindian   = mean(pct_amerindian, na.rm = T),
      avg_pct_nativehawaii = mean(pct_nativehawaii, na.rm = T),
      avg_pct_tworaces     = mean(pct_tworaces, na.rm = T),
      avg_pct_unknownrace  = mean(pct_unknownrace, na.rm = T),
      avg_pct_nonres       = mean(pct_nonres, na.rm = T))
 return(race_by_state)
}

#Q.3.4
race_by_state2 <- create_race_table(csv_dir, "2012")

## -----------------------------------------------------------------------------
## Part IV: Pipeable functions
## -----------------------------------------------------------------------------

#Q4.1 First, create the following objects from race_by_state by using pipes and a tidyverse function:

#object race_by_state_wb that selects wb vars
race_by_state_wb <- race_by_state %>% 
  select(stabbr, 
         avg_pct_white, 
         avg_pct_black) 

#object race_by_state_wpoc that selects wpoc vars
race_by_state_wpoc  <- race_by_state %>% 
  select(stabbr, 
         avg_pct_white, 
         avg_pct_black, 
         avg_pct_latinx, 
         avg_pct_amerindian) 

race_by_state2_wb <- select_race_vars(race_by_state2,avg_pct_white,avg_pct_black)

#Q. 4.2 creating a function similiar to above
select_race_vars <- function(df, ...) {
  subset_df <- df %>% 
    select(stabbr, ...)#select stabbr and the arbitrary# race variables to select from df
    return(subset_df) #returns subsetted df
}

#Q 4.3
#function to create race_by_state2_wb that selects wb vars WITHOUT USING PIPES
race_by_state2_wb <- select_race_vars(df = race_by_state2,  #note: don't need "df =" kept it to remind me
                                      avg_pct_white,
                                      avg_pct_black)

#function to create race_by_state2_wb that selects wpoc vars WITH USING PIPES
race_by_state2_wpoc <- race_by_state2 %>% 
  select_race_vars(avg_pct_white, 
                   avg_pct_black, 
                   avg_pct_latinx, 
                   avg_pct_amerindian)


#Q 4.4
#a new object race_by_state_pivot where you pivot race_by_state_wpoc from wide to long with certain specifications.

race_by_state_pivot <- race_by_state_wpoc %>% 
  pivot_longer(cols = starts_with("avg_pct"), #selecting the cols that start with avg_pct for efficiency 
              names_to = "race",
              names_prefix = "avg_pct_", #drop the avg_pct_ prefix.
              values_to = "percentage")


#Q 4.5  Plotting Year 1

png(file.path(plots_dir, "plot2013.png")) 
print(ggplot(race_by_state_pivot, aes(x=race, y=percentage, color=race)) +  
        geom_jitter(width=0.2))
dev.off()

#Q 4.6  
#Function name: plot_race_figure() 
#• Function argument: df, dir_name, & plot_name
#• Function body:  – Pivot the input df into tidy format (Hint: Remember that the columns in df may vary depending on which race variables were selected, so your code should not be dependent on             specific race variable names)
# - Plot the pivoted dataframe and save the figure in a file called plot_name inside the specified dir_name
#– Returns: The first argument df (so the function remains pipeable)

plot_race_figure <- function(df, dir_name, plot_name) {
  df <- df %>% 
    pivot_longer(cols = starts_with("avg_pct"), 
                 names_to = "race",
                 names_prefix = "avg_pct_",
                 values_to = "percentage")
  png(file.path(dir_name, plot_name)) #could've named plots_dir as dir_name
  print(ggplot(df, aes(x=race, y=percentage, color=race)) +  
          geom_jitter(width=0.2))
  dev.off()
  df
}


#Q 4.7
#plotting year 2 aka 2012 from function
plot_race_figure(race_by_state2_wpoc, plots_dir, "plot2012.png")


#Q. 4.8
#creating a plot from another year, but choosing different race vars.

create_race_table(csv_dir, 2018) %>% 
    select_race_vars(avg_pct_black, 
                     avg_pct_latinx, 
                     avg_pct_amerindian) %>% 
     plot_race_figure(plots_dir, "plot2018_q4_8sjc.png")


## -----------------------------------------------------------------------------
## Extra credit - ggplot
## -----------------------------------------------------------------------------

png(file.path(plots_dir, 'ps8_bonusplot_CousinsSarahJ.png'))
ggplot(race_by_state_pivot, aes(x=stabbr, y=percentage, fill=race)) + 
  geom_bar(stat="identity") +
  theme(axis.text.y = element_text(size = 6))   +
  theme(legend.key.size = unit(.25, 'cm'), legend.title = element_text(size=10)) +
  ggtitle("Figure 1. Average percent of students' race or ethnicity by State")+
  labs(y = "Percentage", fill="Race/Ethnicity", x="State") +
  scale_fill_discrete(labels = c("American Indian", "Black", "Latinx", "White"))+
  coord_flip()
dev.off()

## -----------------------------------------------------------------------------
## GIT TERMINAL COMMANDS // STEPS
## -----------------------------------------------------------------------------
#setup new branch
Git checkout dev_Cousins_SarahJ_ps8 

#from dev branch [[check to see what actually needs to be pushed once complete]]
git add ps8_Cousins_SarahJ.R
git add data/csv_data/ef2012a_rv.csv
git add data/csv_data/ef2012a.csv
git add data/csv_data/ef2013a_rv.csv
git add data/csv_data/ef2013a.csv
git add data/csv_data/hd2012.csv
git add data/dictionaries/ef2013a.xlsx
git add data/dictionaries/hd2013a.xlsx
git add data/plots/ps8_bonusplot_CousinsSarahJ.png
git add data/plots/plot2018_q4_8sjc.png
git add data/plots/plot2012.png
git add data/plots/plot2013.png
git commit -m "inital commit and merge"
#switch to main, pull, merge and push
git checkout main
git pull
git merge dev_Cousins_SarahJ_ps8
git push --set-upstream dev_Cousins_SarahJ_ps8 main

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
