#combining samehour and tophour new dataframes,graphs, analyses
library(blabr)
source("sixseven_data_aggregation.R")
#these four feathers are all you need for all the new same/top figs (plus stuff from main agg script)
# they are created by the tophour_sixseven_dataprep&agg.R & samehour_sixseven_dataprep&agg.R scripts)
#the specific stats for each sub-part (same and top) are in those separate scripts.

TOPHOURsixseven_basiclevel_home_data_agg<- read_feather("data/TOPHOURsixseven_basiclevel_home_data_agg_feather_04_16_18")
TOPHOURsixseven_basiclevel_home_data<- read_feather("data/TOPHOURsixseven_basiclevel_home_data_04_16_18")
TOPHOUR_marker <- read_feather("data/TOPHOUR_marker_04_16_18")
SAMEHOURsixseven_basiclevel_home_data_agg<- read_feather("data/SAMEHOURsixseven_basiclevel_home_data_agg_feather_04_16_18")
SAMEHOURsixseven_basiclevel_home_data<- read_feather("data/SAMEHOURsixseven_basiclevel_home_data_04_16_18")
SAMEHOUR_marker <- read_feather("data/SAMEHOUR_marker_04_16_18")

# samehour various dataframes for figs & stats -------------------------------------
SAMEHOURaudio_ag <- SAMEHOURsixseven_basiclevel_home_data_agg%>%
  #corresponding vid is missing
  dplyr::filter(audio_video=="audio" & SubjectNumber!="17_06")%>%
  setNames(paste0('a_', names(.)))%>%
  rename(subj=a_subj,
         month = a_month,
         SubjectNumber= a_SubjectNumber,
         audio_video = a_audio_video)

SAMEHOURsixseven_spreadAV <- SAMEHOURsixseven_basiclevel_home_data_agg%>%
  ungroup() %>% 
  dplyr::filter(audio_video=="video")%>%
  setNames(paste0('v_', names(.)))%>%
  rename(subj=v_subj,
         month = v_month,
         SubjectNumber= v_SubjectNumber,
         audio_video = v_audio_video)%>%
  dplyr::select(-audio_video) %>% 
  left_join(SAMEHOURaudio_ag)

SAMEHOURsixseven_spreadAV_collapsed <- SAMEHOURsixseven_spreadAV %>% 
  dplyr::select(-month) %>% 
  group_by(subj) %>% 
  summarise_all(mean, na.rm=T)

# tophour various dataframes for figs & stats -------------------------------------
TOPHOURaudio_ag <- TOPHOURsixseven_basiclevel_home_data_agg%>%
  #corresponding vid is missing
  dplyr::filter(audio_video=="audio" & SubjectNumber!="17_06")%>%
  setNames(paste0('a_', names(.)))%>%
  rename(subj=a_subj,
         month = a_month,
         SubjectNumber= a_SubjectNumber,
         audio_video = a_audio_video)

TOPHOURsixseven_spreadAV <- TOPHOURsixseven_basiclevel_home_data_agg%>%
  ungroup() %>% 
  dplyr::filter(audio_video=="video")%>%
  setNames(paste0('v_', names(.)))%>%
  rename(subj=v_subj,
         month = v_month,
         SubjectNumber= v_SubjectNumber,
         audio_video = v_audio_video)%>%
  dplyr::select(-audio_video) %>% 
  left_join(TOPHOURaudio_ag)


TOPHOURsixseven_spreadAV_collapsed <- TOPHOURsixseven_spreadAV %>% 
  dplyr::select(-month) %>% 
  group_by(subj) %>% 
  summarise_all(mean, na.rm=T)


# combo_peak_hour stuff ---------------------------------------------------

TOPHOURcountvals_long <- TOPHOURsixseven_basiclevel_home_data_agg %>% 
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


TOPHOURcountvals_long_collapsed <- TOPHOURcountvals_long %>% 
  ungroup() %>% 
  dplyr::select(-month) %>% 
  group_by(subj, count_meas, audio_video, meas_type) %>% 
  summarise_all(mean, na.rm=T) %>% 
  mutate(meas_type_fig = fct_recode(meas_type, Nspeakers="speaker_num"))#forgraph


SAMEHOURcountvals_long <- SAMEHOURsixseven_basiclevel_home_data_agg %>% 
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


SAMEHOURcountvals_long_collapsed <- SAMEHOURcountvals_long %>% 
  ungroup() %>% 
  dplyr::select(-month) %>% 
  group_by(subj, count_meas, audio_video, meas_type) %>% 
  summarise_all(mean, na.rm=T) %>% 
  mutate(meas_type_fig = fct_recode(meas_type, Nspeakers="speaker_num"))#forgraph

timeslices_countvals_long_collapsed <- SAMEHOURcountvals_long_collapsed %>% 
  mutate(timeslice = ifelse(audio_video=="video","video","same")) %>% 
  bind_rows(TOPHOURcountvals_long_collapsed %>% filter(audio_video=="audio") %>% mutate(timeslice ="top")) %>% 
  bind_rows(countvals_long_collapsed %>% filter(audio_video=="audio") %>% mutate(timeslice ="audio_day")) %>% 
  mutate(hr_day = factor(ifelse(timeslice=="audio_day", "day","hour")),
         timeslice = factor(timeslice))

# top10 & top100 all timeslices ----------------------------------------------------

TOPHOURoverall_top10 <-TOPHOURsixseven_basiclevel_home_data%>%
  group_by(audio_video, object)%>%
  summarise(n = n(),
            nfams = n_distinct(subj)) %>% 
  top_n(10,n) %>% 
  arrange(audio_video, -n)

SAMEHOURoverall_top10 <-SAMEHOURsixseven_basiclevel_home_data%>%
  group_by(audio_video, object)%>%
  summarise(n = n(),
            nfams = n_distinct(subj)) %>% 
  top_n(10,n) %>% 
  arrange(audio_video, -n)


timeslices_overall_top10<- SAMEHOURoverall_top10 %>%  
  mutate(timeslice = ifelse(audio_video=="video","video_hr","audio_samehr")) %>% 
  bind_rows(TOPHOURoverall_top10 %>% filter(audio_video=="audio") %>% mutate(timeslice ="audio_tophr")) %>% 
  bind_rows(overall_top10 %>% filter(audio_video=="audio") %>% mutate(timeslice="audio_day"))

timeslices_overall_top10_nfams <- timeslices_overall_top10 %>% 
  group_by(timeslice) %>% 
  summarise(min_nfams = min(nfams),
            max_nfams = max(nfams),
            mean_freq = mean(n),
            sd_freq = sd(n),
            mean_nfams = mean(nfams),
            sd_nfams = sd(nfams))

timeslices_top100av <- SAMEHOURsixseven_basiclevel_home_data%>%
  mutate(timeslice = ifelse(audio_video=="video","video_hr","audio_samehr")) %>% 
  bind_rows(TOPHOURsixseven_basiclevel_home_data %>% 
              filter(audio_video=="audio" & object %in% top100av$object) %>% 
              mutate(timeslice ="audio_tophr")) %>%  
 # bind_rows(sixseven_basiclevel_home_data %>% filter(audio_video=="audio") %>% mutate(timeslice="audio_day")) %>% 
  mutate(timeslice = as.factor(timeslice)) %>% 
  filter(object %in% top100av$object) %>% 
  group_by(timeslice, object)%>%
  tally()%>%
  top_n(100,n) %>% 
  bind_rows(top100av %>% filter(audio_video=="audio") %>% mutate(timeslice="audio_day"))%>% 
  dplyr::select(-audio_video)

timeslices_top100av_spread <- timeslices_top100av %>% 
  spread(timeslice, n,fill = 0) %>% 
  group_by(object) %>% 
  mutate(aud_vid_sum = sum(audio_day,audio_samehr,audio_tophr, video_hr, na.rm=T)) %>% 
  arrange(-aud_vid_sum)

timeslices_top100av_spread_nozeros<- timeslices_top100av_spread %>% 
  filter(audio_day!=0 & video_hr!=0 & audio_tophr!=0 & audio_samehr!=0) 






# df to plot hour onsets and offsets on one graph -------------------------------------------------------------------
combo_sametopmarker <- SAMEHOUR_marker %>% 
  left_join(TOPHOUR_marker) %>% gather(timetype,startstop, ms_start_samehour:ms_end_tophour)
combo_sametopmarker_wide <- SAMEHOUR_marker %>% 
  left_join(TOPHOUR_marker) 

#filter(combo_sametopmarker, audiorec_startime > video_start_time & timetype=="ms_start_samehour")
#overlap between tophour and samehour, n=15, but actually n=12 bc three were constrained to be that way bc 
#video started before audio
top_same_overlappers <- combo_sametopmarker_wide %>% 
  mutate(overlap_SHTH = ifelse((ms_start_samehour>=ms_start_tophour &
                                  ms_start_samehour<(ms_start_tophour+(60*60*1000)))|
                                 (ms_start_tophour >= ms_start_samehour &
                                    ms_start_tophour<(ms_start_samehour+(60*60*1000))),T, F)) %>% 
  filter(overlap_SHTH==T)#nb: 25_07 & 12_06 & 37_06 had manually adjusted start times to hour1 bc SH was before audio recordings



SAMEHOUR_Avars_with_NAs <- SAMEHOURsixseven_spreadAV %>% 
  dplyr::select(subj,
                a_numtypes, v_numtypes, a_numtokens, v_numtokens, a_numspeakers, v_numspeakers,a_MOT, v_MOT, a_FAT, v_FAT, a_d, v_d, a_q, v_q, a_i, v_i, a_s, v_s, a_r, v_r, a_n, v_n, a_y_op, v_y_op) %>% 
  group_by(subj) %>% 
  summarise_if(is.numeric, funs(sum)) %>% 
  mutate_if(is.numeric, funs(na_if(., 0))) %>%
  mutate(v_FAT = ifelse(subj=="27", 0, v_FAT), # this is a hack so that 27 isn't counted bc that subj had no FAT
         a_FAT = ifelse(subj=="27", 0, a_FAT)) %>%  # this is a hack so that 27 isn't counted bc that subj had no FAT 
  summarise_if(is.numeric, funs(round(sum(is.na(.))/length(.),2)))%>%
  mutate(v_FAT = round(v_FAT *44/43,2)) %>%  # this is a hack so that 27 isn't counted bc that subj had no FAT
  dplyr::select_if(any_vars(.>0)) %>% 
  dplyr::select(-starts_with("v_")) %>% 
  rename("SameA: Singing"=a_s,
         "SameA: Reading"=a_r,
         "SameA: Fathers"=a_FAT)

TOPHOUR_Avars_with_NAs <-TOPHOURsixseven_spreadAV %>% 
  dplyr::select(subj,
                a_numtypes, v_numtypes, a_numtokens, v_numtokens, a_numspeakers, v_numspeakers,a_MOT, v_MOT, a_FAT, v_FAT, a_d, v_d, a_q, v_q, a_i, v_i, a_s, v_s, a_r, v_r, a_n, v_n, a_y_op, v_y_op) %>% 
  group_by(subj) %>% 
  summarise_if(is.numeric, funs(sum)) %>% 
  mutate_if(is.numeric, funs(na_if(., 0))) %>%
  mutate(v_FAT = ifelse(subj=="27", 0, v_FAT), # this is a hack so that 27 isn't counted bc that subj had no FAT
         a_FAT = ifelse(subj=="27", 0, a_FAT)) %>%  # this is a hack so that 27 isn't counted bc that subj had no FAT 
  summarise_if(is.numeric, funs(round(sum(is.na(.))/length(.),2)))%>%
  mutate(v_FAT = round(v_FAT *44/43,2)) %>%  # this is a hack so that 27 isn't counted bc that subj had no FAT
  dplyr::select_if(any_vars(.>0)) %>% 
  dplyr::select(-starts_with("v_")) %>% 
  rename("TopA: Singing"=a_s,
         "TopA: Reading"=a_r,
         "TopA: Fathers"=a_FAT,
         "TopA: Mothers" =a_MOT)

all_vars_with_NAs <- TOPHOUR_Avars_with_NAs %>% 
    bind_cols(SAMEHOUR_Avars_with_NAs) %>% 
    bind_cols(vars_with_NAs) %>% gather(key = "Measure", value = "Prop.Missing") %>% 
  separate(Measure, sep = ": ", into = c("Time Sample","Measure")) %>% 
  #arrange(Measure,-Prop.Missing) %>%
  spread(Measure, Prop.Missing) %>% 
  dplyr::select(`Time Sample`, Fathers, Mothers, Reading, Singing, Imperatives)
