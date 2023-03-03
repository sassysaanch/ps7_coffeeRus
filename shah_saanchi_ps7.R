################################################################################
##
## [ PROJ ] < Problem set #7 >
## [ FILE ] < Homework Edu 260B >
## [ AUTH ] < Saanchi Shah/ sassysaanch >
## [ INIT ] < 2/27/2023 >
##
################################################################################

## ---------------------------
## libraries
## ---------------------------

library(tidyverse)
library(stringr)
library(readr)
library(ggplot2)

## ---------------------------
## directory paths
## ---------------------------
# Do not create the directories yet
plots_dir <- file.path("./plots")
data_dir <- file.path("./data")
csv_dir <- file.path("./data/csv_data")
dict_dir <- file.path("./data/dictionaries")
stata_dir <- file.path("./data/stata_files")



## -----------------------------------------------------------------------------
## Part 1 - Label each question using comments
## -----------------------------------------------------------------------------
# check if directory exists
dir.exists(data_dir)

dir.create(data_dir)

# check again if the direcotry exists

dir.exists(data_dir)

#. If data_dir exists, print out Already have directory:, followed by the name of data_dir
# • Else, print out Creating new directory:, followed by the name of data_dir. Also use
# dir.create() to create the data_dir

# create an if else loop 

if (dir.exists(data_dir)) {
  writeLines("Already have directory: data_dir")
} else if (dir.create(data_dir)) {
  writeLines("Creating new directory: data_dir")
}

# now creating my own function

make_dir <- function(dir_name) {
  # function body
  if (dir.exists(dir_name)) {
    writeLines(str_c("Already have directory: ", dir_name))
  } else if (dir.create(dir_name)) {
    writeLines(str_c("Creating new directory: ",  dir_name))
  }
}

 
# let's call this new function
make_dir(dict_dir)
make_dir(csv_dir)
make_dir(plots_dir)
make_dir(stata_dir)

  
## -----------------------------------------------------------------------------
## Part 2 - Label each question using comments
## -----------------------------------------------------------------------------

url <- 'https://nces.ed.gov/ipeds/datacenter/data/'
file_dirs <- c(csv_dir, dict_dir, stata_dir)
suffixes <- c('', '_Dict', '_Stata')
extensions <- c('.csv', '.xlsx', '.do')

# Group member 2
files <- c('hd2016', 'hd2015', 'hd2014')

# Create data_url object
data_url <- for (i in files){
  print(i)
 data_url[i] <- str_c(url, i, ".zip")
}

# create data_zipfile

data_url <- for (i in files){
  print(files)
  data_url <- str_c(url, i, ".zip")
  for(j in file_dirs){
  print(j)
    data_zipfile <- file.path(j, str_c("/", i, ".zip")) #zip file destination
    writeLines(str_c(j, "/", i, ".zip"))
    download.file(data_url, destfile = data_zipfile)}
}


file.exists(data_zipfile)

# create an if else block to download files

if(file.exists(data_zipfile) == FALSE){
  download.file(data_url, destfile = data_zipfile)
  writeLines(str_c("Downloading file : ", data_zipfile))
} else {
  writeLines(str_c("Already have file: ", data_zipfile))
}

# Let's unzip the files

for (i in file_dirs) {
  for (j in files) {
    # for (k in extensions){
      data_unzipped <- i
      filename <- str_c(j, ".zip")
      print(filename)
      print(data_unzipped)
      unzip(zipfile = file.path(data_unzipped, filename),
            exdir = data_unzipped)
    }
  }
# }


file.exists(data_unzipped) # none of the files exist yet and the next time it does

# create an if else block for these zip files now

if(file.exists(data_zipfile) == FALSE || file.exists(data_unzipped) == FALSE){
  download.file(data_url, destfile = data_zipfile)
  unzip(zipfile = file.path(data_unzipped, filename),
        exdir = data_unzipped)
  writeLines(str_c("Downloading file and unzipping file : ", data_unzipped))
} else if (file.exists(data_unzipped) == TRUE){
  writeLines(str_c("Already have file: ", data_zipfile, data_unzipped))
}

# creating a function
download_file <- function(dir_name, file_name, file_suffix, file_extension) {
# function body
data_url <- str_c(url, file_name, file_suffix, ".zip")

data_zipfile <- str_c(dir_name, file_name, file_suffix, ".zip")

data_unzipped <- str_c(dir_name, "/", file_name, file_extension)

if(file.exists(data_zipfile) == FALSE || file.exists(data_unzipped) == FALSE){
  download.file(data_url, destfile = data_zipfile)
  unzip(zipfile = data_zipfile,
        exdir = dir_name)
  writeLines(str_c("Downloading file and unzipping file : ", data_unzipped))
} else if (file.exists(data_unzipped) == TRUE){
  writeLines(str_c("Already have file: ", data_zipfile, data_unzipped))
}
}

# Question 7
download_file(file_dirs[3], files[1], suffixes[3], extensions[3])

file_dirs <- c(csv_dir, dict_dir, stata_dir)
suffixes <- c('', '_Dict', '_Stata')
extensions <- c('.csv', '.xlsx', '.do')
files <- c('hd2016', 'hd2015', 'hd2014')

# question 8

for (i in seq_along(files)) {
  for (j in seq_along(file_dirs))  {
        download_file(file_dirs[j], files[i], suffixes[j], extensions[j])
      }
    }




## -----------------------------------------------------------------------------
## Part 3 - Label each question using comments
## -----------------------------------------------------------------------------

# Question1

nums <- vector()
save_plot <- function(file_name) {
  df <- read_csv(file.path(csv_dir, str_c(file_name, ".csv"))) %>% 
  transmute(hbcu = if_else(HBCU == 1, 1, 0), 
            # or use mutate here and then re-pipe to use select()
            tribal = if_else(TRIBAL == 1, 1, 0),
            hospital = if_else(HOSPITAL == 1, 1, 0)) 
for(i in seq_along(df)){
  nums[[i]] <- str_c(sum(df[i]))
}
  png(file.path(plots_dir, str_c(file_name, '.png')))
  print(ggplot(data.frame(nums), aes(seq_along(nums), nums)) +
          geom_bar(stat = 'identity') +
          scale_x_continuous(breaks = seq_along(df), labels = names(df)) +
          xlab(NULL) + ylab(NULL))
  dev.off()
}

# Question 2

for (i in files){
  save_plot(file_name = i)
}


## -----------------------------------------------------------------------------
## Extra Credit
## -----------------------------------------------------------------------------

# storing 2016 data
hd2016 <- read_csv("./data/csv_data/hd2016.csv")

png(file.path(plots_dir, 'extra_credit.png'))
print(hd2016 %>% 
  filter(GROFFER != -3) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = OBEREG, after_stat(prop),  fill = as.factor(GROFFER), position = "dodge")) +
  labs(fill = "Graduate degrees offered", labels = c("Yes", "No")) +
  xlab("Bureau of economic analysis regions") +
  scale_x_continuous(breaks = seq(0, 9)) +
  labs(
    caption = 
"0 - US Service schools
1 - New England 
2 - Mid East 
3 - Great Lakes 
4 - Plains 
5 - Southeast 
6 - Southwest 
7 - Rocky Mountains 
8 - Far West 
9 - Outlying areas 
", 
labels = c("Yes", "No")) + 
  ggtitle(
    "Proportion of schools offering graduate degrees/certificates by OBEREG") +
  scale_fill_discrete(labels = c("Yes", "No")))
dev.off()
  


## -----------------------------------------------------------------------------
## Part IV - Label each question using comments
## -----------------------------------------------------------------------------
# github issue: https://github.com/anyone-can-cook/rclass2_student_issues_w23/issues/312
# github response: https://github.com/anyone-can-cook/rclass2_student_issues_w23/issues/306

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
