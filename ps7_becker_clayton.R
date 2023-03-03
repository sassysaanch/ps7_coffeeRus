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


### Bonus Plot ###

plot_subset <- df %>% mutate(
  location = case_when(LOCALE == "11" ~ "City, L", 
                       LOCALE == "12" ~ "City, M",
                       LOCALE == "13" ~ "City, S",
                       LOCALE == "21" ~ "Suburb, L", 
                       LOCALE == "22" ~ "Suburb, M", 
                       LOCALE == "23" ~ "Suburb, S",
                       LOCALE == "31" ~ "Town, Fringe", 
                       LOCALE == "32" ~ "Town, Distant",
                       LOCALE == "33" ~ "Town, Remote", 
                       LOCALE == "41" ~ "Rural, Fringe",
                       LOCALE == "42" ~ "Rural, Distant",
                       LOCALE == "43" ~ "Rural, Remote"),
  medical = MEDICAL
) %>% select(location, medical, INSTNM) %>% drop_na() %>% filter(medical == 1)


png(file.path(plots_dir, "becker_bonus_plot.png"), width = 600, height = 600, units = "px")
plot_subset %>% group_by(location) %>%
  summarise(medical = sum(medical)) %>% 
  ggplot(mapping = aes(x = location, y = medical)) +
  geom_col() +
    geom_text(aes(label = medical, vjust = -0.5)) +
  ylab("Medical Degree Offered") + xlab("School Location") + 
  labs(title = "Number of Institutions Offering a Medical Degree by Geography", 
       caption = "Based on NCES IPEDS data for 2017") +
  scale_x_discrete() +
  theme_minimal()
dev.off()

plot_subset %>% group_by(location) %>%
  summarise(medical = sum(medical))

# I am still unable to get these column numbers to be the number in the column rather than 
# the total sum of the number of observations in the data frame. I will one day figure this out

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
