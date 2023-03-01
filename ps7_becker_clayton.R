################################################################################
##
## [ PROJ ] < Problem set 7 >
## [ FILE ] < ps7_becker_clayton >
## [ AUTH ] < cnbecker14 >
## [ INIT ] < February 25th, 2023 >
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


# Question 5

dir.exists(data_dir)
dir.create(data_dir)

# Question 6

if (dir.exists(data_dir)) {
  writeLines(str_c("Already have directory: ", data_dir))
} else {
  writeLines(str_c("Creating new directory: ", data_dir))
  dir.create(data_dir)
}

# Question 7 

make_dir <- function(dir_name) {
  if (dir.exists(dir_name)) {
    writeLines(str_c("Already have directory: ", dir_name))
  } else {
    writeLines(str_c("Creating new directory: ", dir_name))
    dir.create(dir_name)
  }
}

# Question 8

directories <- c(data_dir, plots_dir, csv_dir, dict_dir, stata_dir)

for (i in directories) {
  make_dir(i)
}

## -----------------------------------------------------------------------------
## Part 2 - Label each question using comments
## -----------------------------------------------------------------------------

# Question 1 

url <- 'https://nces.ed.gov/ipeds/datacenter/data/'

file_dirs <- c(csv_dir, dict_dir, stata_dir)
suffixes <- c('', '_Dict', '_Stata')
extensions <- c('.csv', '.xlsx', '.do')
files <- c('hd2019', 'hd2018', 'hd2017')

# Question 2

data_url <- str_c(url, files[1], suffixes[1],".zip")

data_zipfile <- str_c(file_dirs[1], files[1], ".zip")

file.exists(data_zipfile)

download.file(url = data_url, destfile = data_zipfile)

# Question 3 

if(file.exists(data_zipfile) == FALSE) {
  writeLines(str_c("Dowloading file: ", data_zipfile))
  download.file(url = data_url, destfile = data_zipfile)
} else {
  writeLines(str_c("Already have file: ", data_zipfile))
}

# Question 4

data_unzipped <- str_c(file_dirs[1], files[1], extensions[1])

file.exists(data_unzipped)

unzip(data_zipfile, exdir = csv_dir)

# Question 5

if(file.exists(data_zipfile) == FALSE) {
  writeLines(str_c("Dowloading file: ", data_zipfile, " & Unzipping file: ", data_unzipped))
  download.file(url = data_url, destfile = data_zipfile)
  unzip(data_zipfile, exdir = csv_dir)
} else if (file.exists(data_unzipped) == FALSE) {
  writeLines(str_c("Unzipping file: ", data_unzipped))
  unzip(data_zipfile, exdir = csv_dir)
} else {
  writeLines(str_c("Already have files: ", data_zipfile, " ", data_unzipped))
}

# Question 6 

download_file <- function(files, file_dirs, suffixes , extensions) {
  data_url <- str_c(url, files, suffixes,".zip")
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

download_file(files = files[1], 
              file_dirs = file_dirs[1], 
              suffixes = suffixes[1], 
              extensions = extensions[1])

download_file(files = files[1], 
              file_dirs = file_dirs[2], 
              suffixes = suffixes[2], 
              extensions = extensions[2])

download_file(files = files[1], 
              file_dirs = file_dirs[3], 
              suffixes = suffixes[3], 
              extensions = extensions[3])

# Question 8

for (i in 1:length(files)) {
  for (j in 1:length(file_dirs)) {
        download_file(files = files[i], 
                      file_dirs = file_dirs[j], 
                      suffixes = suffixes[j], 
                      extensions = extensions[j])
    }}

## -----------------------------------------------------------------------------
## Part 3 - Label each question using comments
## -----------------------------------------------------------------------------

# Question 1

1. In this section, you will create a function for saving plots to your `plots_dir`. 
The plot will be the same as the one from Part IV of problem set 6. You can copy your code from there 
and adapt it to work inside your function:
  
  - **Function name**: `save_plot()`
- **Function argument**: `file_name` (the name of the file you are reading in)
- **Function body**: Read in the CSV data from your `csv_dir` file path directory. The data file would 
be your `file_name` input plus the `.csv` extension. Your function should perform the same data manipulations 
you did in Part IV of problem set 6 to generate the `nums` vector containing sums for `HBCU`, `TRIBAL`, and `HOSPITAL`. 
Remember to create the `nums` vector using the vector() function outside of the loop similar to Part IV question 5. 
Lastly, paste the following code inside your function body to save the plot:

nums <- vector()

save_plot <- function(files) {
  df <- read.csv(file = file.path(csv_dir, str_c(files, ".csv")))
  df_subset <- df %>% 
  mutate(
    HBCU = if_else(HBCU == 1, 1, 0),
    TRIBAL = if_else(TRIBAL == 1, 1, 0),
    HOSPITAL = if_else(HOSPITAL == 1, 1, 0)) %>% select(HBCU, TRIBAL, HOSPITAL)
  for (i in 1:length(df_subset)) {
    nums[[i]] <- str_c(sum(df_subset[[i]]))
  }
  png(file.path(plots_dir, str_c(files, '.png')))
  print(ggplot(data.frame(nums), aes(seq_along(nums), nums)) +
          geom_bar(stat = 'identity') +
          scale_x_continuous(breaks = seq_along(df_subset), labels = names(df_subset)) +
          xlab(NULL) + ylab(NULL))
  dev.off() }

# Question 2 

for (i in files) {
  files_save <- str_c(i)
  save_plot(files = files_save)
}


### Bonus Plot###


## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
