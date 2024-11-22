library(here)
library(tidyverse)
library(jsonlite)
library(testthat)

processed_data_directory <- here("..","data","processed_data")
file_name <- "naive_theories"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv")))

#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id) %>%
  count()

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
  group_by(participant) %>%
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

#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))

#quick look
# subj_summary <- processed_data %>% 
#   group_by(participant,T1,T2) %>% 
#   summarize(avg=mean(is_right,na.rm=T),mean_rt=mean(rt[is_right==1&rt<20000]))
# overall_summary <- subj_summary  %>% 
#   group_by(T1,T2) %>% 
#   summarize(N=n(),
#             avg_acc=mean(avg),
#             avg_rt=mean(mean_rt))
# 
# ggplot(subj_summary,aes(as.character(T1),mean_rt))+
#   geom_boxplot()+
#   geom_jitter(width=0.1)+
#   facet_wrap(~as.character(T2))
# 
# ggplot(subj_summary,aes(as.character(T1),avg))+
#   geom_boxplot()+
#   geom_jitter(width=0.1)+
#   facet_wrap(~as.character(T2))
