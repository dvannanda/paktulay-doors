# load packages
library(tidyverse)
library(here)
library(readxl)

# import data
filepath = "data/raw_data - Copy.xlsx"

tl0 <- read_xlsx(here(filepath), sheet = "tinggi lebar")  # measurements (length x width)
ns0 <- read_xlsx(here(filepath), sheet = "nomor sisi")    # number needed per edge
materi0 <- read_xlsx(here(filepath), sheet = "materi")    # materials
unit0 <- read_xlsx(here(filepath), sheet = "model unit")  # units per model

# measurements CLEANING
tl <- 
  tl0 %>% 
  # wide to long dataframe
  pivot_longer(cols = -c(model, tipe),
               names_to = "tipe2",
               values_to = "ukuran") %>% 
  # filter empty spaces 
  dplyr::filter(! is.na(ukuran)) %>% 
  # separate categories 
  separate(tipe2, c("tipe2", "tipe3"), " ") %>% 
  select(-tipe)

tl_extra <-
  tl %>% 
  dplyr::filter(tipe2 == "kusen-extra") %>% 
  separate(tipe3, c("tipe3", "tipe4"), "-")

# nomor sisi CLEANING
ns <- 
  ns0 %>% 
  # wide to long dataframe
  pivot_longer(cols = -model,
               names_to = "tipe2",
               values_to = "perlu per unit") %>% 
  # filter empty spaces 
  dplyr::filter(! is.na(`perlu per unit`)) %>% 
  # separate categories 
  separate(tipe2, c("tipe2", "tipe3"), " ") %>% 
  separate(tipe3, c("tipe3", "tipe4"), "-")

# materi CLEANING
materi <-
  materi0 %>% 
  # wide to long dataframe
  pivot_longer(cols = -model,
               names_to = "tipe2",
               values_to = "materi") %>% 
  # filter empty spaces 
  dplyr::filter(! is.na(materi)) %>%
  # separate categories 
  separate(tipe2, c("tipe2", "tipe3"), " ") %>% 
  separate(tipe3, c("tipe3", "tipe4"), "-")

# unit CLEANING
unit <-
  unit0 %>% 
  select(model, unit)

# tipe (to add main types to df)
tipe <- 
  tl0 %>% 
  group_by(model) %>% 
  count(tipe) %>% 
  select(-n)

# join dataset
df <-
  materi %>% 
  left_join(ns, by = c("model", "tipe2", "tipe3", "tipe4")) %>% 
  left_join(tl, by = c("model", "tipe2", "tipe3")) %>% 
  # fix extra
  left_join(tl_extra, by = c("model", "tipe2", "tipe3", "tipe4")) %>% 
  pivot_longer(cols = starts_with("ukuran."),
               names_to = "temp2",
               values_to = "ukuran") %>% 
  dplyr::filter(! is.na(ukuran)) %>% 
  select(- starts_with("temp")) %>% 
  # add main type
  left_join(tipe, by = "model") %>% 
  relocate(tipe, .after = model) %>% 
  # add unit
  left_join(unit, by = "model")

# save as csv
write_csv(df, file = "data/tidy-data.csv")