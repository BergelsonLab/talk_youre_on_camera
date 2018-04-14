#just the TOP audio hour and the video hour
library(blabr)
source("sixseven_data_aggregation.R")


TOPHOUR_marker <- read_csv("data/top_hour_bl_vs_lena.csv") %>% 
  filter(bl_lena=="bl") %>% 
  rename(SubjectNumber=file,
         ms_start_tophour = onset) %>% 
  mutate(ms_end_tophour = ms_start_tophour+(60*60*3600)) %>% 
  dplyr::select(-time, -bl_lena)

TOPHOURsixseven_basiclevel_home_data <-sixseven_basiclevel_home_data %>% 
  left_join(TOPHOUR_marker) %>%
  filter(audio_video=="video" | (audio_video=="audio" & 
                                   onset >= ms_start_tophour & 
                                   onset < ms_end_tophour)) %>%
  mutate(SubjectNumber=as.factor(SubjectNumber)) 


summary(TOPHOURsixseven_basiclevel_home_data)

TOPHOURsixseven_basiclevel_home_data_agg <- big_aggregate(TOPHOURsixseven_basiclevel_home_data)

summary(TOPHOURsixseven_basiclevel_home_data_agg, maxsum=50)


#TOPHOURoverall agg feather
#
# two feathers, agg, and not-agg, six/sev month only, tophour only
write_feather(TOPHOURsixseven_basiclevel_home_data_agg, "data/TOPHOURsixseven_basiclevel_home_data_agg_feather_04_13_18")
write_feather(TOPHOURsixseven_basiclevel_home_data, "data/TOPHOURsixseven_basiclevel_home_data_04_13_18")
write_feather(TOPHOUR_marker, "data/TOPHOUR_marker_04_13_18")

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

# tophour stats -----------------------------------------------------------
THw1 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_numtokens, TOPHOURsixseven_spreadAV_collapsed$a_numtokens,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "THw_numtok")
THw2 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_numtypes, TOPHOURsixseven_spreadAV_collapsed$a_numtypes,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "THw_numtyp")
THw3 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_numspeakers, TOPHOURsixseven_spreadAV_collapsed$a_numspeakers,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "THw_numsp")
THw4 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_MOT, TOPHOURsixseven_spreadAV_collapsed$a_MOT,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "THw_MOT")
THw5 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_FAT, TOPHOURsixseven_spreadAV_collapsed$a_FAT,conf.int=T, paired = T)%>% tidy() %>% mutate(comp = "THw_FAT")
THw6 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_d, TOPHOURsixseven_spreadAV_collapsed$a_d,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "THw_d")
THw7 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_q, TOPHOURsixseven_spreadAV_collapsed$a_q,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "THw_q")
THw8 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_r, TOPHOURsixseven_spreadAV_collapsed$a_r,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "THw_r")
THw9 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_s, TOPHOURsixseven_spreadAV_collapsed$a_s,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "THw_s")
THw10 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_i, TOPHOURsixseven_spreadAV_collapsed$a_i,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "THw_i")
THw11 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_n, TOPHOURsixseven_spreadAV_collapsed$a_n,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "THw_n")
THw12 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_y_op, TOPHOURsixseven_spreadAV_collapsed$a_y_op,conf.int=T, paired = T)%>%  tidy() %>% mutate(comp = "THw_yop")

THws <- bind_rows(THw1, THw2, THw3, THw4, THw5, THw6, THw7, THw8, THw9, THw10, THw11, THw12) %>% 
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
THws %>% filter(pval_adj<.05)

THpropw4 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_propd,
                        TOPHOURsixseven_spreadAV_collapsed$a_propd, conf.int=T, paired = T)%>%
  tidy()%>% mutate(comp = "THpropw_d")
THpropw5 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_propi,
                        TOPHOURsixseven_spreadAV_collapsed$a_propi, conf.int=T, paired = T)%>%
  tidy()%>% mutate(comp = "THpropw_i")
THpropw6 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_propr,
                        TOPHOURsixseven_spreadAV_collapsed$a_propr, conf.int=T, paired = T)%>%
  tidy()%>% mutate(comp = "THpropw_r")
THpropw7 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_propq,
                        TOPHOURsixseven_spreadAV_collapsed$a_propq, conf.int=T, paired = T)%>%
  tidy()%>% mutate(comp = "THpropw_q")
THpropw8 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_props,
                        TOPHOURsixseven_spreadAV_collapsed$a_props, conf.int=T, paired = T)%>%
  tidy()%>% mutate(comp = "THpropw_s")
THpropw9 <- wilcox.test(TOPHOURsixseven_spreadAV_collapsed$v_propn,
                        TOPHOURsixseven_spreadAV_collapsed$a_propn, conf.int=T, paired = T)%>%
  tidy()%>% mutate(comp = "THpropw_n")
THpropws <- bind_rows(THpropw4, THpropw5, THpropw6, THpropw7, THpropw8, THpropw9) %>%  
  mutate(pval_adj = p.adjust(p.value, method = "holm"))
THpropws %>% filter(pval_adj<.05)