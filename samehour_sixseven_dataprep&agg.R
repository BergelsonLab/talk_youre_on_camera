#Same hour as video was, in audio file
library(readxl)
library(lubridate)
source("sixseven_data_aggregation.R")

#haven't gotten this to work yet, placeholder

homevisittime <- read_csv("data/06_07_homevisitappttime.csv") %>% 
  dplyr::select(-Date) %>% 
  mutate(SubjectNumber = factor(SubjectNumber)) 


SAMEHOUR_marker <- read_csv("data/audio_start_times_6_7.csv") %>% #89 lines bc it includes subj who dropped out early 24_06
  mutate(date_its = as.Date(start),
         time_its) %>% 
  dplyr::select(-start)
  rename(SubjectNumber=file,
         SAMEHOUR_onset = onset) %>% 
  left_join(vidtime) %>% 
  mutate(vidtime_ms = total_min*60*1000) %>% 
  dplyr::select(-time, -bl_lena, -audio_video, -total_min) #time is just 'ms' for milliseconds and bl_lena is now all 'bl'

SAMEHOURsixseven_basiclevel_home_data <-sixseven_basiclevel_home_data %>% 
  left_join(SAMEHOUR_marker) %>% 
  group_by(SubjectNumber, audio_video) %>% 
  filter(audio_video=="video" | audio_video=="audio" & 
           (onset >= SAMEHOUR_onset & 
              (SAMEHOUR_onset+(60*60*1000))>=onset))


summary(SAMEHOURsixseven_basiclevel_home_data)
# test <- SAMEHOURsixseven_basiclevel_home_data %>% 
#   group_by(SubjectNumber, audio_video) %>% 
#   summarise(length_min = (max(offset)-min(onset))/60/1000,
#             vidlength = mean(vidtime_ms/1000/60),
#             n())
# filter(sixseven_basiclevel_home_data, SubjectNumber=="01_07", audio_video=="audio") %>% summary()
# filter(sixseven_basiclevel_home_data, SubjectNumber=="17_06", audio_video=="audio") %>% summary()
# filter(SAMEHOURsixseven_basiclevel_home_data, SubjectNumber=="36_06" & audio_video=="audio") %>% summary()
# filter(sixseven_basiclevel_home_data, SubjectNumber=="32_07") %>% summary()
# filter(SAMEHOURsixseven_basiclevel_home_data, SubjectNumber=="03_06") %>% summary()
# filter(sixseven_basiclevel_home_data, SubjectNumber=="03") %>% summary()
# aggregating the home data -----------------------------------------------

#we make lots of little dataframes first
#the majority of these aggregate over subj, month, and audio_video


# num words in 6 month experiment -----------------------------------------
#note: established possible basic levels for other versions of these words
# #by crawling like so:
# all_basiclevel_home_data %>%
#    distinct(basic_level)%>%
#    filter(grepl("^[b]", x = basic_level))%>%
#     arrange(basic_level)%>%as.data.frame()


SAMEHOURnum_experimentwords <- SAMEHOURsixseven_basiclevel_home_data %>%
  count_experimentwords()


# MOT and FAT count -------------------------------------------------------
SAMEHOURsix_to_seventeen_home_FAT_MOT_count <- SAMEHOURsixseven_basiclevel_home_data %>%
  count_mot_fat()

# utterance type count ----------------------------------------------------
SAMEHOURsix_to_seventeen_home_utt_count <-  SAMEHOURsixseven_basiclevel_home_data %>%
  count_utterance()


# object present count ----------------------------------------------------
SAMEHOURsix_to_seventeen_home_op <- SAMEHOURsixseven_basiclevel_home_data %>%
  count_object_present()

SAMEHOURsix_to_seventeen_home_op_exp <- SAMEHOURsixseven_basiclevel_home_data %>%
  object_present_exp()


# device and toy use count ------------------------------------------------
SAMEHOURsix_to_seventeen_home_device_count <-  SAMEHOURsixseven_basiclevel_home_data %>%
  count_device_and_toy()


# few versions of kid talk info -------------------------------------------

#chi tokens
SAMEHOURsix_to_seventeen_home_chi_count <-  SAMEHOURsixseven_basiclevel_home_data %>%
  count_chi()

#chi types
SAMEHOURsix_to_seventeen_home_chi_type_count <-  SAMEHOURsixseven_basiclevel_home_data %>%
  count_chi_types()

# noun production onset age
SAMEHOURsix_to_seventeen_home_noun_chi_onset <- SAMEHOURsixseven_basiclevel_home_data %>%
  chi_noun_onset()



#STUCK HERE BC OF BIG_AGGREGATE ERROR




# finally, big aggregation of our little datasets -------------------------

SAMEHOURsixseven_basiclevel_home_data_agg <- big_aggregate(SAMEHOURsixseven_basiclevel_home_data)

summary(SAMEHOURsixseven_basiclevel_home_data_agg, maxsum=50)


#SAMEHOURoverall agg feather
#
# two feathers, agg, and not-agg, six/sev month only, SAMEHOUR only
write_feather(SAMEHOURsixseven_basiclevel_home_data_agg %>% filter(month=="06" | month=="07"), "data/SAMEHOURsixseven_basiclevel_home_data_agg_feather_04_09_18")
write_feather(SAMEHOURsixseven_basiclevel_home_data%>% filter(month=="06" | month=="07"), "data/SAMEHOURsixseven_basiclevel_home_data_04_09_18")
