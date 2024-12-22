library(tidyverse)
library(janitor)
library(googlesheets4)


# Import data from Google Drive
gs4_deauth()

energy_id <- "1CNRi8UfgNt-lu7Df7TsQeSsXS5N6XMBSP3UTyt5lloM"
energy <- read_sheet(energy_id, sheet = "Raw Data")


# Cleaner column names for easy manipulation
energy <- 
  energy |> clean_names()


# Convert year columns which contains strings to numeric year columns
energy <- 
  energy |> 
  # Remove brackets and contents to keep only year
  mutate(year = str_replace_all(year, "\\[.*?\\]", "")) |> 
  # Remove "est.". Some year values are like this: 2018 est.
  mutate(year = str_replace_all(year, "est\\.", "")) |> 
  # Trim white space and convert year to numeric vector 
  mutate(year = as.numeric(trimws(year))) |> 
  # Same operations for as_of column which also contains years
  mutate(as_of = str_replace_all(as_of, "\\[.*?\\]", "")) |> 
  mutate(as_of = str_replace_all(as_of, "est\\.", "")) |> 
  mutate(as_of = as.numeric(trimws(as_of)))
  
# Remove rows with missing values and asterisks from location column
energy <- energy  |> 
  filter(!grepl("\\*", location))

colnames(energy)
