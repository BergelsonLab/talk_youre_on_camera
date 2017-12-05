library(tidyverse)
library(feather)
library(ggrepel)
library(broom)
library(forcats)
library(MASS)
library(Hmisc)
library(lmerTest)


sixseven_basiclevel_home_data <- read_feather("data/sixsevmonth_basiclevel_home_data_feather")%>%
#sixseven_basiclevel_home_data <- get_all_basiclevel() %>% 
 #  dplyr::filter(month%in% c("06","07"))%>%
   droplevels()
#source("seedlings_basiclevels_dataprep_sixseven.R")


sixseven_basiclevel_home_data_agg <- read_feather("data/all_basiclevel_home_data_agg_feather10-31-17")%>%
  dplyr::filter(month %in% c("06","07"))%>%
#sixseven_basiclevel_home_data_agg <- read_feather("data/sixsevmonth_basiclevel_home_data_agg_feather")%>%
  droplevels() %>% 
  dplyr::select(-contains("exp"), -TOY, -CHI, -CHItypes, -prop_tech, -tech, -prop_parent, -sum_prop_ut, -noun_chi_onset, -posttalk, -ent_subj_av)
  #dplyr::select(subj:num_exp_types, d, q, n, s, r,i, TOY:tech, propd, propq, propn, props, propr, propi, everything())


summary(sixseven_basiclevel_home_data_agg$subj, maxsum = 50)

#vid lengths
vidtime <- read_csv("data/vidlengths_subject_files_1-30-16.csv") %>% 
  separate(file, into = c("SubjectNumber", "extra"), sep = 5, remove = T, extra = drop) %>% 
  dplyr::select(-extra) %>% 
  rename(total = `length(s)`) %>% 
  mutate(total_min = round(total/60,1)) %>% 
  arrange(-total_min) %>% 
  dplyr::filter(SubjectNumber %in%sixseven_basiclevel_home_data$SubjectNumber) %>% 
  mutate(audio_video="video") %>% 
  dplyr::select(-total)

ggplot(vidtime, aes(total_min, SubjectNumber))+geom_point()+theme_bw(base_size=8)


audtime <- read_csv("data/06_07_audio_silence_and_total_time.csv") %>%
  rename(SubjectNumber = file) %>% 
  mutate(SubjectNumber = factor(SubjectNumber),
         audio_video="audio",
         total_min = round(wav_time/1000/60),
         total_hr = round(wav_time/1000/60/60),
         tot_nosilsk = round((wav_time-silence-skip)/1000/60),
         tot_nosil = round((wav_time - silence)/1000/60),
         silence_min = round(silence/1000/60),
         skip_min = round(skip/1000/60)) %>% 
  arrange(-tot_nosil)
summary(audtime)
qplot(audtime$tot_nosil)

sixseven_basiclevel_home_data_agg <- audtime %>% 
  dplyr::select(SubjectNumber, audio_video, total_min, tot_nosil, tot_nosilsk) %>% 
  bind_rows(vidtime) %>% 
  left_join(sixseven_basiclevel_home_data_agg) %>% 
  mutate(SubjectNumber = factor(SubjectNumber),
         audio_video = factor(audio_video))

#we want a spread version of the data too
audio_ag <- sixseven_basiclevel_home_data_agg%>%
#corresponding vid is missing
    dplyr::filter(audio_video=="audio" & SubjectNumber!="17_06")%>%
  setNames(paste0('a_', names(.)))%>%
  rename(subj=a_subj,
         month = a_month,
         SubjectNumber= a_SubjectNumber,
         audio_video = a_audio_video)

sixseven_spreadAV <- sixseven_basiclevel_home_data_agg%>%
  dplyr::filter(audio_video=="video")%>%
  setNames(paste0('v_', names(.)))%>%
  rename(subj=v_subj,
         month = v_month,
         SubjectNumber= v_SubjectNumber,
         audio_video = v_audio_video)%>%
  dplyr::select(-audio_video, -v_tot_nosilsk, -v_tot_nosil) %>% 
  left_join(audio_ag)

#month spread

six_spreadmonth <- sixseven_basiclevel_home_data_agg%>%
  dplyr::filter(month=="06")%>%
  dplyr::select(-SubjectNumber, -month)%>%
  setNames(paste0('six_', names(.)))%>%
  rename(subj=six_subj,
      # month = six_month,
       audio_video = six_audio_video)
summary(six_spreadmonth)

sixseven_spreadmonth <- sixseven_basiclevel_home_data_agg%>%
  dplyr::filter(month=="07" & !(audio_video=="video" & subj=="17"))%>%
  dplyr::select(-SubjectNumber, -month)%>%
  setNames(paste0('sev_', names(.)))%>%
  rename(subj=sev_subj,
       #month = sev_month,
       audio_video = sev_audio_video)%>%
  left_join(six_spreadmonth)

sixseven_aud <- sixseven_spreadmonth %>% dplyr::filter(audio_video=="audio")
sixseven_vid <- sixseven_spreadmonth %>% dplyr::filter(audio_video=="video")

sixseven_spreadAV_normmin <- sixseven_spreadAV %>%
  dplyr::select(
    subj,
    month,
    a_total_min,
    a_tot_nosilsk,
    a_tot_nosil,
    v_total_min,
    a_numtypes,
    v_numtypes,
    a_numtokens,
    v_numtokens,
    a_numspeakers,
    v_numspeakers,
    a_MOT,
    v_MOT,
    a_FAT,
    v_FAT,
    a_d,
    v_d,
    a_q,
    v_q,
    a_i,
    v_i,
    a_s,
    v_s,
    a_r,
    v_r,
    a_n,
    v_n,
    a_y_op,
    v_y_op
  ) %>%
  ungroup() %>%
  rename(vid_total_min = v_total_min,
         aud_tot_nosil = a_tot_nosil) %>% 
  mutate_at(vars(starts_with("v_")),funs(./vid_total_min)) %>% 
  mutate_at(vars(starts_with("a_")), funs(./aud_tot_nosil))

six_spreadAV_normmin <- subset(sixseven_spreadAV_normmin, month=="06")
sev_spreadAV_normmin <- subset(sixseven_spreadAV_normmin, month=="07")


sixseven_spreadAV_normminNA <- sixseven_spreadAV_normmin %>%  mutate_if(is.numeric, funs(na_if(., 0)))
six_spreadAV_normminNA <-subset(sixseven_spreadAV_normminNA, month=="06")
sev_spreadAV_normminNA <- subset(sixseven_spreadAV_normminNA, month=="07")

propvals_med <- sixseven_basiclevel_home_data_agg %>%
  dplyr::select(month, audio_video, prop_op, prop_mom, prop_dad, propd, propi, propn, propq, propr, props, type_token_ratio) %>%
  group_by(month, audio_video) %>%
  #summarise_if(is.numeric, funs(min, max, mean, median))
  summarise_if(is.numeric, funs(median)) %>%
  gather(prop_meas, medianval, prop_op:type_token_ratio)
# 
propvals_mean <- sixseven_basiclevel_home_data_agg %>%
  dplyr::select(month, audio_video, prop_op, prop_mom, prop_dad, propd, propi, propn, propq, propr, props, type_token_ratio) %>%
  group_by(month, audio_video) %>%
  #summarise_if(is.numeric, funs(min, max, mean, median))
  summarise_if(is.numeric, funs(mean)) %>%
  gather(prop_meas, meanval, prop_op:type_token_ratio)

propvals_mean_spreadAV <- propvals_mean %>% 
  spread(audio_video, meanval) %>% 
  mutate(a_boost = audio-video) 

propvals_long <- sixseven_basiclevel_home_data_agg %>% 
  dplyr::select(month, audio_video, subj, prop_op, prop_mom, prop_dad, propd, propq, propn, props, propr, propi, type_token_ratio) %>% 
  group_by(month, audio_video, subj) %>% 
  gather(prop_meas, propval, prop_op:type_token_ratio) %>% 
  mutate(prop_meas=factor(prop_meas),
         prop_meas = fct_relevel(prop_meas,
                                 c("prop_op", "prop_mom", "prop_dad", "propd", "propq", "propn", "props", "propr", "propi","type_token_ratio")),
         meas_type = fct_collapse(prop_meas,
                                  speaker = c("prop_mom","prop_dad"),
                                  utt = c("propd","propq","propn","props","propr","propi"),
                                  quant = "type_token_ratio",
                                  op = "prop_op"))

countvals_long <- sixseven_basiclevel_home_data_agg %>% 
  dplyr::select(month, audio_video, subj, y_op, MOT, FAT, d, q, n, s, r, i, numtypes, numtokens, numspeakers) %>% 
  group_by(month, audio_video, subj) %>% 
  gather(count_meas, countval, y_op:numspeakers) %>% 
  mutate(count_meas=factor(count_meas),
         count_meas = fct_relevel(count_meas,
                                  c("y_op", "MOT", "FAT", "d", "q", "n", "s", "r", "i", "numtypes", "numtokens", "numspeakers")),
         meas_type = fct_collapse(count_meas,
                                  speaker = c("MOT","FAT"),
                                  speaker_num = c("numspeakers"),
                                  utt = c("d","q","n","s","r","i"),
                                  quant = c("numtypes","numtokens"),
                                  op = "y_op"))
countvals_long_norm <- sixseven_spreadAV_normmin %>% 
  dplyr::select(-a_total_min, a_tot_nosilsk) %>% 
  gather(key = norm_meas, value = normval, a_numtypes:v_y_op) %>% 
  mutate(audio_video=as.factor(ifelse(norm_meas %in% c("a_y_op","a_MOT","a_FAT","a_d","a_q","a_n","a_s","a_r","a_i","a_numtypes","a_numtokens","a_numspeakers"), "audio", ifelse(norm_meas %in% c("v_y_op","v_MOT","v_FAT","v_d","v_q","v_n","v_s","v_r","v_i","v_numtypes","v_numtokens","v_numspeakers"),
                                                                                                                                                                                 "video","FIXTHIS")))) %>% 
  mutate(norm_meas=factor(norm_meas),
         norm_meas = fct_collapse(norm_meas,
                                  y_op= c("v_y_op","a_y_op"),
                                  MOT = c("v_MOT","a_MOT"),
                                  FAT = c("v_FAT","a_FAT"),
                                  d = c("v_d","a_d"),
                                  q = c("v_q","a_q"),
                                  n = c("v_n","a_n"),
                                  s = c("v_s","a_s"),
                                  r = c("v_r","a_r"),
                                  i = c("v_i","a_i"),
                                  numtypes = c("v_numtypes","a_numtypes"),
                                  numtokens = c("v_numtokens","a_numtokens"),
                                  numspeakers = c("v_numspeakers","a_numspeakers")),
         norm_meas = fct_relevel(norm_meas,
                                 c("y_op", "MOT", "FAT", "d", "q", "n", "s", "r", "i", "numtypes", "numtokens", "numspeakers")),
         meas_type = fct_collapse(norm_meas,
                                  speaker = c("MOT","FAT"),
                                  speaker_num = c("numspeakers"),
                                  utt = c("d","q","n","s","r","i"),
                                  quant = c("numtypes","numtokens"),
                                  op = "y_op"))

# word tallies ------------------------------------------------------------

tally_month_av<- sixseven_basiclevel_home_data %>%
  group_by(object,audio_video, month) %>%
  tally() %>%
  arrange(-n)

totaln_objectwords <- sixseven_basiclevel_home_data %>% 
  group_by(object) %>%
  tally() %>% nrow()

totaln_once_objectwords <- sixseven_basiclevel_home_data %>% 
  group_by(object) %>%
  tally() %>% filter(n==1) %>% nrow()

totaln_bl <- sixseven_basiclevel_home_data %>% 
  group_by(basic_level) %>%
  tally() %>% nrow()

totaln_once_bl <- sixseven_basiclevel_home_data %>% 
  group_by(basic_level) %>%
  tally() %>% filter(n==1) %>% nrow()

# top words dataframes ----------------------------------------------------


overall_month_top10 <-sixseven_basiclevel_home_data%>%
  group_by(audio_video, month, object)%>%
  summarise(n = n(),
            nfams = n_distinct(subj)) %>% 
  top_n(10,n) %>% 
  arrange(audio_video, month, -n)


#collapsing month
top100av <- sixseven_basiclevel_home_data%>%
  group_by(audio_video, object)%>%
  tally()%>%
  top_n(100,n)

top100av_spread <- top100av %>% 
  spread(audio_video, n,fill = 0) %>% 
  group_by(object) %>% 
  mutate(aud_vid_sum = sum(audio,video, na.rm=T)) %>% 
  arrange(-aud_vid_sum)

top100av_spread %>%
  mutate(zeroA_V= ifelse(audio==0|video==0,T,F)) %>%
  group_by(zeroA_V) %>%
  tally()

top100av_spread_nozeros<- top100av_spread %>% 
  filter(audio!=0 & video!=0) 

top100av67 <- sixseven_basiclevel_home_data%>%
  group_by(audio_video, month,object)%>%
  tally() %>% 
  #   group_by(object)%>%
  #   summarise(n = n(),
  #             nfams = n_distinct(subj)) %>% 
  #   #filter(nfams>11) %>% #only including words that 25% the families heard in that recording-type and month
  #   #dplyr::select(-nfams) %>% #but then removing this var for further spreading below
  #   #ungroup() %>% 
  #   #group_by(audio_video, month) %>%
  top_n(100,n) %>%
  #   arrange(-nfams, n) %>% 
  #   filter(!(nfams==1 & n==1)& nfams>1) %>% 
  arrange(audio_video, month, -n)


top100av67_avspread <- top100av67 %>% 
  spread(audio_video, n, fill = 0) %>% 
  group_by(object, month) %>% 
  mutate(aud_vid_sum = sum(audio,video, na.rm=T)) %>% 
  arrange(-aud_vid_sum)

top100audio_monthspread <- top100av67_avspread %>% 
  dplyr::select(-video, -aud_vid_sum) %>% 
  spread(month, audio, fill = 0) %>% 
  rename(six = `06`,
         seven = `07`) %>% 
  filter(!(six==0 & seven==0)) %>% 
  mutate(audio_video="audio")

top100video_monthspread <- top100av67_avspread %>% 
  dplyr::select(-audio, -aud_vid_sum) %>% 
  spread(month, video, fill = 0) %>% 
  rename(six = `06`,
         seven = `07`) %>% 
  filter(!(six==0 & seven==0)) %>% 
  mutate(audio_video="video")

top100_monthspread <- bind_rows(top100audio_monthspread, top100video_monthspread)