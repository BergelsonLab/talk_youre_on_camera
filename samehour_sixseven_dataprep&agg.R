#Same hour as video was, in audio file
library(blabr)
#source("sixseven_data_aggregation.R")

sixseven_basiclevel_home_data <- read_feather("data/sixsevmonth_basiclevel_home_data_feather_04_16_18")%>%
  droplevels()

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
write_feather(SAMEHOURsixseven_basiclevel_home_data_agg , "data/SAMEHOURsixseven_basiclevel_home_data_agg_feather_04_16_18")
write_feather(SAMEHOURsixseven_basiclevel_home_data, "data/SAMEHOURsixseven_basiclevel_home_data_04_16_18")
write_feather(SAMEHOUR_marker, "data/SAMEHOUR_marker_04_16_18")


