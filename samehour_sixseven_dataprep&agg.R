#Same hour as video was, in audio file
library(blabr)
source("sixseven_data_aggregation.R")


homevisittime <- read_csv("data/06_07_homevisitappttime.csv") %>% 
  dplyr::select(-Date, -HomeVisitApptTime) %>% 
  rename(video_start_time = HomeVisitApptTime24h)

SAMEHOUR_marker <- read_csv("data/audio_start_times_6_7.csv") %>% #89 lines bc it includes subj who dropped out early 24_06
  dplyr::select(-start) %>% 
  rename(SubjectNumber=file,
         audiorec_startime = startime_time) %>% 
  right_join(homevisittime) %>% 
  mutate(ms_start_samehour = ifelse((video_start_time-audiorec_startime)<0, 0, as.numeric((video_start_time-audiorec_startime)*1000)),#3 videos started before audio
         ms_end_samehour = ms_start_samehour+(60*60*3600),
         SubjectNumber = factor(SubjectNumber))
as.data.frame(SAMEHOUR_marker)  
summary(SAMEHOUR_marker)


SAMEHOURsixseven_basiclevel_home_data <- sixseven_basiclevel_home_data %>%
  left_join(SAMEHOUR_marker) %>% 
  filter(audio_video=="video" | (audio_video=="audio" & 
           onset >= ms_start_samehour & 
             onset < ms_end_samehour)) %>%
  mutate(SubjectNumber=as.factor(SubjectNumber)) 
summary(SAMEHOURsixseven_basiclevel_home_data)

# aggregating the same hour data -----------------------------------------------
SAMEHOURsixseven_basiclevel_home_data_agg <- big_aggregate(SAMEHOURsixseven_basiclevel_home_data)

summary(SAMEHOURsixseven_basiclevel_home_data_agg, maxsum=50)
summary(SAMEHOURsixseven_basiclevel_home_data_agg$subj)

#SAMEHOURoverall agg feather

# two feathers, agg, and not-agg, six/sev month only, SAMEHOUR only
write_feather(SAMEHOURsixseven_basiclevel_home_data_agg , "data/SAMEHOURsixseven_basiclevel_home_data_agg_feather_04_13_18")
write_feather(SAMEHOURsixseven_basiclevel_home_data, "data/SAMEHOURsixseven_basiclevel_home_data_04_13_18")
write_feather(SAMEHOUR_marker, "data/SAMEHOUR_marker_04_13_18")
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

# samehour stats ----------------------------------------------------------
SHw1 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_numtokens, SAMEHOURsixseven_spreadAV_collapsed$a_numtokens,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "SHw_numtok")
SHw2 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_numtypes, SAMEHOURsixseven_spreadAV_collapsed$a_numtypes,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "SHw_numtyp")
SHw3 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_numspeakers, SAMEHOURsixseven_spreadAV_collapsed$a_numspeakers,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "SHw_numsp")
SHw4 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_MOT, SAMEHOURsixseven_spreadAV_collapsed$a_MOT,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "SHw_MOT")
SHw5 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_FAT, SAMEHOURsixseven_spreadAV_collapsed$a_FAT,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "SHw_FAT")
SHw6 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_d, SAMEHOURsixseven_spreadAV_collapsed$a_d,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "SHw_d")
SHw7 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_q, SAMEHOURsixseven_spreadAV_collapsed$a_q,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "SHw_q")
SHw8 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_r, SAMEHOURsixseven_spreadAV_collapsed$a_r,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "SHw_r")
SHw9 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_s, SAMEHOURsixseven_spreadAV_collapsed$a_s,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "SHw_s")
SHw10 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_i, SAMEHOURsixseven_spreadAV_collapsed$a_i,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "SHw_i")
SHw11 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_n, SAMEHOURsixseven_spreadAV_collapsed$a_n,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "SHw_n")
SHw12 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_y_op, SAMEHOURsixseven_spreadAV_collapsed$a_y_op,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "SHw_yop")

SHws <- bind_rows(SHw1, SHw2, SHw3, SHw4, SHw5, SHw6, SHw7, SHw8, SHw9, SHw10, SHw11, SHw12) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
SHws %>% filter(pval_adj<.05)

SHpropw4 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_propd,
                        SAMEHOURsixseven_spreadAV_collapsed$a_propd, conf.int=T, paired = T)%>%
  tidy()%>% mutate(comp = "SHpropw_d")
SHpropw5 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_propi,
                        SAMEHOURsixseven_spreadAV_collapsed$a_propi, conf.int=T, paired = T)%>%
  tidy()%>% mutate(comp = "SHpropw_i")
SHpropw6 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_propr,
                        SAMEHOURsixseven_spreadAV_collapsed$a_propr, conf.int=T, paired = T)%>%
  tidy()%>% mutate(comp = "SHpropw_r")
SHpropw7 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_propq,
                        SAMEHOURsixseven_spreadAV_collapsed$a_propq, conf.int=T, paired = T)%>%
  tidy()%>% mutate(comp = "SHpropw_q")
SHpropw8 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_props,
                        SAMEHOURsixseven_spreadAV_collapsed$a_props, conf.int=T, paired = T)%>%
  tidy()%>% mutate(comp = "SHpropw_s")
SHpropw9 <- wilcox.test(SAMEHOURsixseven_spreadAV_collapsed$v_propn,
                        SAMEHOURsixseven_spreadAV_collapsed$a_propn, conf.int=T, paired = T)%>%
  tidy()%>% mutate(comp = "SHpropw_n")
SHpropws <- bind_rows(SHpropw4, SHpropw5, SHpropw6, SHpropw7, SHpropw8, SHpropw9) %>%  
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
SHpropws %>% filter(pval_adj<.05)

# same_cor_AVtop_collapsed <-cor.test(timeslices_top100av_spread$audio_samehr,
#                                timeslices_top100av_spread$video_hr,
#                                method = "kendall") %>% tidy()
# top_cor_AVtop_collapsed <-cor.test(timeslices_top100av_spread$audio_tophr,
#                                     timeslices_top100av_spread$video_hr,
#                                     method = "kendall") %>% tidy()

