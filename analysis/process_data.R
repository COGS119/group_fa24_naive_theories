library(here)
library(tidyverse)
library(jsonlite)
library(testthat)

processed_data_directory <- here("..","data","processed_data")
file_name <- "naive_theories"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv"))) %>%
  rename(participant_id=participant)

#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

#extract reflections
reflection_1 <- exp_data %>%
  filter(!is.na(reflection1)) %>%
  select(random_id,reflection1) %>%
  rename(reflection_1=reflection1)

#extract reflections
reflection_2 <- exp_data %>%
  filter(!is.na(reflection2)) %>%
  select(random_id,reflection2) %>%
  rename(reflection_2=reflection2)

exp_data <- exp_data %>%
  left_join(reflection_1) %>%
  left_join(reflection_2)

#filter and select relevant data
processed_data <- exp_data %>%
  filter(!is.na(Category)) %>%
  select(-success,-c(reflection1:file_name)) %>%
  group_by(participant_id) %>%
  mutate(
    trial_number=seq(n())
  ) %>%
  relocate(
    trial_number,.after="trial_index"
  ) %>%
  mutate(
    is_right = ifelse(correct,1,0)
  ) %>%
  relocate(
    is_right,.after="correct"
  ) 

#filter participant ids
filter_ids <- c(
  "1","12","1234","a1","as","nm","p4","p6"
)

processed_data <- processed_data %>%
  mutate(participant_id = trimws(tolower(participant_id))) %>%
  #fix some ids
  mutate(
    participant_id = case_when(
      participant_id == "herson" ~ "heron",
      participant_id == "p73" ~ "giraffe",
      participant_id == "2341" ~ "porcupine",
      TRUE ~ participant_id
    )
  ) %>%
  filter(!(participant_id %in% filter_ids))

#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
