################################################################################
##
## [ PROJ ] < Problem set #7 >
## [ FILE ] < p7_Cousins_SarahJ >
## [ AUTH ] < Sarah J. Cousins/SJC0usins >
## [ INIT ] < Due 3/3/23 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(expss)

## ---------------------------
## directory paths
## ---------------------------
plots_dir <- file.path('.', 'plots')
data_dir <- file.path('.', 'data')
csv_dir <- file.path('.',  'data/csv_data')
dict_dir <- file.path('.',  'data/dictionaries')
stata_dir <- file.path('.',  'data/stata_files')


## -----------------------------------------------------------------------------
## Part 1
## -----------------------------------------------------------------------------

#Q 1.5
dir.exists(data_dir) #to check if data_dir exists 
#[1] FALSE
dir.create(data_dir) #function to create a data folder within  ps7_coffeerus dirrectory. 
dir.exists(data_dir) #to check if data_dir exists 
#[1] TRUE

#Q 1.6 
if (dir.exists(data_dir)) {
  writeLines(str_c("Already have directory:", data_dir, sep=" "))
} else if (dir.create(data_dir)){
  writeLines(str_c("Creating new directory:", data_dir, sep=" "))
}
#OUTPUT: Already have directory: ./data

#alternative - For some reason else only works if the dir.create occurs last.
if (dir.exists(data_dir)) {
  writeLines(str_c("Already have directory:", data_dir,  sep=" "))
} else{
  writeLines(str_c("Creating new directory:", data_dir,  sep=" "))
  (dir.create(data_dir))
}
#OUTPUT: Already have directory: ./data

#Q 1.7
make_dir <- function(dir_name) {
  if (dir.exists(dir_name)) {
    writeLines(str_c("Already have directory:", dir_name, sep=" "))
  } else if (dir.create(dir_name)) {
    writeLines(str_c("Creating new directory:", dir_name, sep=" "))
  }
}

#Q 1.8 - Call your make_dir() function with all the directory path objects you created earlier (i.e., data_dir, plots_dir, csv_dir, dict_dir, stata_dir). This should create all the remaining directories.
directories <- c(data_dir, plots_dir, csv_dir, dict_dir, stata_dir) 
for (i in directories) {
  make_dir(i)
}
#OUTPUT - Already have directory: ./data
#OUTPUT -Creating new directory: ./plots
#OUTPUT -Creating new directory: ./data/csv_data
#OUTPUT -Creating new directory: ./data/dictionaries
#OUTPUT -Creating new directory: ./data/stata_files

## -----------------------------------------------------------------------------
## Part 2- Downloading and unzipping data
## -----------------------------------------------------------------------------

#Q 2.1
url <- 'https://nces.ed.gov/ipeds/datacenter/data/'
file_dirs <- c(csv_dir, dict_dir, stata_dir)
suffixes <- c('', '_Dict', '_Stata')
extensions <- c('.csv', '.xlsx', '.do')

#Group member 3
files <- c('hd2013', 'hd2008', 'hd2007')

#Q 2.2 Downloading the hd2013 zip after creating url and zipfile
data_url <- str_c(url, files[1], suffixes[1],".zip")
data_url

data_zipfile <- str_c(file_dirs[1], files[1], ".zip")
data_zipfile
file.exists(data_zipfile) 
#[1] FALSE
download.file(url = data_url, destfile = data_zipfile)
file.exists(data_zipfile)
#[1] TRUE

#Q 2.3
if(!file.exists(data_zipfile)) {
  writeLines(str_c("Downloading file:", sep=" ", data_zipfile))
  download.file(url = data_url, destfile = data_zipfile)
} else {
  writeLines(str_c("Already have file:", sep=" ", data_zipfile))
}

#Q 2.4
data_unzipped <- file.path(file_dirs[1], str_c(files[1], extensions[1]))

file.exists(data_unzipped)

#only unzipping the csv file
unzip(zipfile = data_zipfile, exdir = file_dirs[1])
data_unzipped
file.exists(data_unzipped)

#Q 2.5
if(!file.exists(data_zipfile)) {
  writeLines(str_c("Dowloading file: ", data_zipfile, " & Unzipping file: ", data_unzipped))
  download.file(url = data_url, destfile = data_zipfile)
  unzip(data_zipfile, exdir = csv_dir)
} else if (!file.exists(data_unzipped)) {
  writeLines(str_c("Unzipping file: ", data_unzipped))
  unzip(data_zipfile, exdir = csv_dir)
} else {
  writeLines(str_c("Already have files: ", data_zipfile, " ", data_unzipped))
}                 

#Q 2.6

download_file <- function(files, file_dirs, suffixes , extensions) {
  data_url <- str_c(url, files, suffixes,".zip")
  data_zipfile <- str_c(file_dirs, files, ".zip")
  data_unzipped <- str_c(file_dirs, files, extensions)
  if(!file.exists(data_zipfile)) {
    writeLines(str_c("Dowloading file: ", data_zipfile, " & Unzipping file: ", data_unzipped))
    download.file(url = data_url, destfile = data_zipfile)
    unzip(data_zipfile, exdir = file_dirs)
  } else if (!file.exists(data_unzipped)) {
    writeLines(str_c("Unzipping file: ", data_unzipped))
    unzip(data_zipfile, exdir = file_dirs)
  } else {
    writeLines(str_c("Already have files: ", data_zipfile, " ", data_unzipped))
  }
}

#Q 2.7
#Downloading 2013 for all file types
#2013 csv
download_file(file_name=files[1], directories=file_dirs[1], file_suffixes=suffixes[1], file_extension=extensions[1])
#OUTPUT - Unzipping file: ./data/csv_datahd2013.csv
#2013 xlsx
download_file(files[1],file_dirs[2], suffixes[2], extensions[2])
#OUTPUT =Unzipping file: ./data/dictionarieshd2013.xlsx

#2013 do file
download_file(files[1], file_dirs[3], suffixes[3], extensions[3])
#OUTPUT - Unzipping file: ./data/stata_fileshd2013.do

#Q 2.8 Downloading+unzipping 2008, 2013, 2007 .xlsx, .do, and .CSV files.
 
for (i in 1:length(files)) {
  for (j in 1:length(file_dirs)) {
    download_file(files = files[i], file_dirs = file_dirs[j], suffixes = suffixes[j], extensions = extensions[j])
  }}


## -----------------------------------------------------------------------------
## Part 3 - Plotting data
## -----------------------------------------------------------------------------

#Q 3.1 creating and saving plots for 2013, 2008 and 2007

save_plot <- function(file_name){
  df <- read.csv(file.path(csv_dir, str_c(file_name, ".csv")))
  df_subset <- df %>% select(HBCU, TRIBAL, HOSPITAL) %>%
    mutate(
      HBCU = if_else(HBCU == 1, 1, 0),
      TRIBAL = if_else(TRIBAL == 1, 1, 0),
      HOSPITAL = if_else(HOSPITAL == 1, 1, 0))
  
  nums<- vector("numeric", length(df_subset))
  
  for (i in 1:length(df_subset)){
    nums[[i]] <- sum(df_subset[[i]])
    writeLines(str_c(nums))
  }
  
  png(file.path(plots_dir, str_c(file_name, '.png')))
  print(ggplot(data.frame(nums), aes(seq_along(nums), nums)) +
          geom_bar(stat = 'identity') +scale_x_continuous(breaks = seq_along(df_subset),
labels = names(df_subset)) + xlab(NULL) + ylab(NULL))
  dev.off()
}

#Q 3.2 Saving all 3 plots to plot file

for (i in files){
  save_plot(i)
}

## -----------------------------------------------------------------------------
## EXTRA CREDIT - BONUS PLOT
## -----------------------------------------------------------------------------

hd2013 <- read_csv("./data/csv_data/hd2013.csv")
hd2013_grad <- hd2013 %>% mutate(GROFFER = if_else(GROFFER == 1, "Yes", "No"))

#should have done below in mutate above
var_lab(hd2013_grad$TRIBAL) = "Tribal College"
val_lab(hd2013_grad$TRIBAL) = num_lab("
             1 Tribal    
             2 Other College" 
)

png(file.path(plots_dir, 'bonusplot_Cousins_SarahJ.png'))
ggplot(data= hd2013_grad, aes(x= GROFFER,  group=TRIBAL)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1, vjust = 1, face = "plain")) +
  theme(legend.key.size = unit(.25, 'cm'), legend.title = element_text(size=10)) +
  ggtitle("Figure 1. Disparities in Graduate Degrees Offered at Tribal Colleges")+
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  scale_fill_discrete(labels = c("Yes", "No"))+
  labs(y = "Percent", fill="Graduate Degree", x="Offers Graduate Degree") +
  facet_grid(~TRIBAL) +
  scale_y_continuous(labels = scales::percent)
dev.off()

## -----------------------------------------------------------------------------
##  POSTING ON GIT
## -----------------------------------------------------------------------------

Replied: https://github.com/anyone-can-cook/rclass2_student_issues_w23/issues/332
Posted: https://github.com/anyone-can-cook/rclass2_student_issues_w23/issues/336 


## -----------------------------------------------------------------------------
## GIT TERMINAL COMMANDS // STEPS
## -----------------------------------------------------------------------------

#initating and linking remote to local
git clone https://github.com/anyone-can-cook/ps7_coffeerus
cd ./ps7_coffeerus
#creating dev branch
git checkout -b dev_Cousins_SarahJ
#blah blah, doing assignment but now ready to send to remote/coffeerus
#confirming i'm on dev branch -- could also use git status
git branch -a
#adding while on my dev branch
git add p7_Cousins_SarahJ.R
git add plots/hd2013.png
git add plots/hd2008.png
git add plots/hd2007.png
git add plots/bonusplot_Cousins_SarahJ.png
git commit -m "inital attempt to merge. wishing myself luck"
#merge to main
git checkout main
git pull
git merge dev
git push --set-upstream dev_Cousins_SarahJ main
#sending up revised r script which was missing my name while on main branch
git pull
git add p7_Cousins_SarahJ.R
git commit -m "r script copy edits"
git push
#sending up data files
git add data/csv_data
git add data/dictionaries
git add data/data_files
git commit -m "sending data files"

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
