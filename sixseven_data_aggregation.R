library(tidyverse)
library(feather)
library(ggrepel)
library(broom)
library(forcats)
library(MASS)
library(Hmisc)

#eb This is where we want to change the code so it gets the latest version of allbasiclevel, and writes out what that version is when rendered.

sixseven_basiclevel_home_data <- read_feather("data/sixsevmonth_basiclevel_home_data_feather")%>%
#   filter(month%in% c("06","07"))%>%
   droplevels()
#source("seedlings_basiclevels_dataprep_sixseven.R")


sixseven_basiclevel_home_data_agg <- read_feather("data/all_basiclevel_home_data_agg_feather10-31-17")%>%
  filter(month%in% c("06","07"))%>%
#sixseven_basiclevel_home_data_agg <- read_feather("data/sixsevmonth_basiclevel_home_data_agg_feather")%>%
  droplevels() %>% 
  dplyr::select(subj:num_exp_types, d, q, n, s, r,i, TOY:tech, propd, propq, propn, props, propr, propi, everything())

summary(sixseven_basiclevel_home_data_agg$subj, maxsum = 50)

#vid lengths
vidtime <- read_csv("data/vidlengths_subject_files_1-30-16.csv") %>% 
  separate(file, into = c("SubjectNumber", "extra"), sep = 5, remove = T, extra = drop) %>% 
  dplyr::select(-extra) %>% 
  rename(total = `length(s)`) %>% 
  mutate(total_min = round(total/60,1)) %>% 
  arrange(-total_min) %>% 
  filter(SubjectNumber %in%sixseven_basiclevel_home_data$SubjectNumber) %>% 
  mutate(audio_video="video") %>% 
  dplyr::select(-total)

#ggplot(vidtime, aes(total_min, SubjectNumber))+geom_point()+theme_bw(base_size=8)

#aud lengths
audtime_two <- read_csv("data/audiotimes.csv") %>% 
  rename(SubjectNumber="file") %>% 
  mutate(total_min = round((wav_time/1000/60),1))
audtime <- read_tsv("data/ACLEW_list_of_corpora - recording_level.tsv") %>%
  rename(SubjectNumber = "lab_internal_subject_id",
         total_min = "length_of_recording") %>% 
  #filter(SubjectNumber %in% sixseven_basiclevel_home_data$SubjectNumber) %>% 
  filter(SubjectNumber %in% audtime_two$SubjectNumber) %>% 
  dplyr::select(SubjectNumber, total_min) %>% 
  mutate(audio_video ="audio",
         total_min = as.numeric(as.character(total_min)))
# clantime <- read_csv("data/clantime_06_07.csv") %>% 
#   separate(file, into = c("SubjectNumber", "extra"), sep = 5, remove = T, extra = drop) %>% 
#   dplyr::select(-extra) %>% 
#   mutate(total_min = total/1000/60,
#          total_hr = total/1000/60/60,
#          awake = total-silence-skip,
#          awake_min = awake/1000/60,
#          silence_min = silence/1000/60,
#          skip_min = skip/1000/60,
#          total_less_sil_min = (total-silence)/1000/60)%>% 
#   arrange(-skip_min)
# clantime %>% 
#   filter(SubjectNumber%in%sixseven_basiclevel_home_data_agg$SubjectNumber) %>%
#   rename(total_min_clan = total_min) %>% 
#   left_join(sixseven_basiclevel_home_data_agg) %>% 
#   dplyr::select(SubjectNumber, total_min_clan, total_min, audio_video, month) %>% 
#   filter(audio_video=="audio" & month=="06") %>% View()

sixseven_basiclevel_home_data_agg <- audtime %>% bind_rows(vidtime) %>% 
  left_join(sixseven_basiclevel_home_data_agg) %>% 
  mutate(SubjectNumber = factor(SubjectNumber),
         audio_video = factor(audio_video))

#we want a spread version of the data too
audio_ag <- sixseven_basiclevel_home_data_agg%>%
  filter(audio_video=="audio" & SubjectNumber!="17_06")%>%
  dplyr::select(-noun_chi_onset, -posttalk)%>%
  setNames(paste0('a_', names(.)))%>%
  rename(subj=a_subj,
         month = a_month,
         SubjectNumber= a_SubjectNumber)

sixseven_spreadAV <- sixseven_basiclevel_home_data_agg%>%
  filter(audio_video=="video")%>%
  dplyr::select(-noun_chi_onset, -posttalk)%>%
  setNames(paste0('v_', names(.)))%>%
  rename(subj=v_subj,
         month = v_month,
         SubjectNumber= v_SubjectNumber)%>%
  left_join(audio_ag)

#month spread

six_spreadmonth <- sixseven_basiclevel_home_data_agg%>%
  filter(month=="06")%>%
  dplyr::select(-noun_chi_onset, -posttalk, -SubjectNumber, -month)%>%
  setNames(paste0('six_', names(.)))%>%
  rename(subj=six_subj,
      # month = six_month,
       audio_video = six_audio_video)
summary(six_spreadmonth)

sixseven_spreadmonth <- sixseven_basiclevel_home_data_agg%>%
  filter(month=="07" & !(audio_video=="video" & subj=="17"))%>%
  dplyr::select(-noun_chi_onset, -posttalk, -SubjectNumber, -month)%>%
  setNames(paste0('sev_', names(.)))%>%
  rename(subj=sev_subj,
       #month = sev_month,
       audio_video = sev_audio_video)%>%
  left_join(six_spreadmonth)

sixseven_aud <- sixseven_spreadmonth %>% filter(audio_video=="audio")
sixseven_vid <- sixseven_spreadmonth %>% filter(audio_video=="video")
