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

8. Finally, use loops to batch download all of the IPEDS files. These will be nested loops, 
similar to Part II of problem set 6. You'll want to loop over each year's data, then loop over each 
type of data to call `download_file()` for each data type. There are multiple ways to do this.

prior to running this code, you may want to delete files you downloaded or unzipped while 
answering previous questions; and good to experiment with how function call differs when you 
"Already have files" vs. not

for (i in files) {
  files_loop <- str_c(i)
  for (o in file_dirs) {
    dirs_loop <- str_c(o)
    for (p in suffixes) {
      suffix_loop <-  str_c(p)
      for (a in extensions) {
        extension_loop <- str_c(a)
        download_file(files = files_loop, 
                      file_dirs = dirs_loop, 
                      suffixes = suffix_loop, 
                      extensions = extension_loop)
      }}}}

## -----------------------------------------------------------------------------
## Part 2 - Label each question using comments
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
