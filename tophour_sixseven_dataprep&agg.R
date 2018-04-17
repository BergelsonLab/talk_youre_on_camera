#just the TOP audio hour and the video hour
library(blabr)
#source("sixseven_data_aggregation.R")

sixseven_basiclevel_home_data <- read_feather("data/sixsevmonth_basiclevel_home_data_feather_04_16_18")%>%
  droplevels()

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
write_feather(TOPHOURsixseven_basiclevel_home_data_agg, "data/TOPHOURsixseven_basiclevel_home_data_agg_feather_04_16_18")
write_feather(TOPHOURsixseven_basiclevel_home_data, "data/TOPHOURsixseven_basiclevel_home_data_04_16_18")
write_feather(TOPHOUR_marker, "data/TOPHOUR_marker_04_16_18")


