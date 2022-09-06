## initialisation
library('dplyr')
library('tidyverse')
library('readxl')
setwd("C:/py/gsa") # Set WD

## Read in data
q <- read_csv("data/raw/Q_halfClean_2.csv")
play <- read_excel("data/cleaned/gameplay.xlsx")

## Rename

play <- play %>% rename(
  dur = time,
)

## Identify players by time
