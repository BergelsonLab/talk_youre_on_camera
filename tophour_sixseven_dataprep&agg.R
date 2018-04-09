#just the TOP audio hour and the video hour
source("sixseven_data_aggregation.R")


tophour_marker <- read_csv("data/top_hour_bl_vs_lena.csv") %>% 
  filter(bl_lena=="bl") %>% 
  rename(SubjectNumber=file,
         tophour_onset = onset) %>% 
  left_join(vidtime) %>% 
  mutate(vidtime_ms = total_min*60*1000) %>% 
  dplyr::select(-time, -bl_lena, -audio_video, -total_min) #time is just 'ms' for milliseconds and bl_lena is now all 'bl'

TOPHOURsixseven_basiclevel_home_data <-sixseven_basiclevel_home_data %>% 
  left_join(tophour_marker) %>% 
  group_by(SubjectNumber, audio_video) %>% 
  filter(audio_video=="video" | audio_video=="audio" & 
                                (onset >= tophour_onset & 
                                (tophour_onset+(60*60*1000))>=onset))
 

summary(TOPHOURsixseven_basiclevel_home_data)
# test <- TOPHOURsixseven_basiclevel_home_data %>% 
#   group_by(SubjectNumber, audio_video) %>% 
#   summarise(length_min = (max(offset)-min(onset))/60/1000,
#             vidlength = mean(vidtime_ms/1000/60),
#             n())
# filter(sixseven_basiclevel_home_data, SubjectNumber=="01_07", audio_video=="audio") %>% summary()
# filter(sixseven_basiclevel_home_data, SubjectNumber=="17_06", audio_video=="audio") %>% summary()
# filter(TOPHOURsixseven_basiclevel_home_data, SubjectNumber=="36_06" & audio_video=="audio") %>% summary()
# filter(sixseven_basiclevel_home_data, SubjectNumber=="32_07") %>% summary()
# filter(TOPHOURsixseven_basiclevel_home_data, SubjectNumber=="03_06") %>% summary()
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


TOPHOURnum_experimentwords <- TOPHOURsixseven_basiclevel_home_data %>%
  count_experimentwords()


# MOT and FAT count -------------------------------------------------------
TOPHOURsix_to_seventeen_home_FAT_MOT_count <- TOPHOURsixseven_basiclevel_home_data %>%
  count_mot_fat()

# utterance type count ----------------------------------------------------
TOPHOURsix_to_seventeen_home_utt_count <-  TOPHOURsixseven_basiclevel_home_data %>%
  count_utterance()


# object present count ----------------------------------------------------
TOPHOURsix_to_seventeen_home_op <- TOPHOURsixseven_basiclevel_home_data %>%
  count_object_present()

TOPHOURsix_to_seventeen_home_op_exp <- TOPHOURsixseven_basiclevel_home_data %>%
  object_present_exp()


# device and toy use count ------------------------------------------------
TOPHOURsix_to_seventeen_home_device_count <-  TOPHOURsixseven_basiclevel_home_data %>%
  count_device_and_toy()


# few versions of kid talk info -------------------------------------------

#chi tokens
TOPHOURsix_to_seventeen_home_chi_count <-  TOPHOURsixseven_basiclevel_home_data %>%
  count_chi()

#chi types
TOPHOURsix_to_seventeen_home_chi_type_count <-  TOPHOURsixseven_basiclevel_home_data %>%
  count_chi_types()

# noun production onset age
TOPHOURsix_to_seventeen_home_noun_chi_onset <- TOPHOURsixseven_basiclevel_home_data %>%
  chi_noun_onset()



#STUCK HERE BC OF BIG_AGGREGATE ERROR




# finally, big aggregation of our little datasets -------------------------

TOPHOURsixseven_basiclevel_home_data_agg <- big_aggregate(TOPHOURsixseven_basiclevel_home_data)

summary(TOPHOURsixseven_basiclevel_home_data_agg, maxsum=50)


#TOPHOURoverall agg feather
#
# two feathers, agg, and not-agg, six/sev month only, tophour only
write_feather(TOPHOURsixseven_basiclevel_home_data_agg %>% filter(month=="06" | month=="07"), "data/TOPHOURsixseven_basiclevel_home_data_agg_feather_04_09_18")
write_feather(TOPHOURsixseven_basiclevel_home_data%>% filter(month=="06" | month=="07"), "data/TOPHOURsixseven_basiclevel_home_data_04_09_18")
