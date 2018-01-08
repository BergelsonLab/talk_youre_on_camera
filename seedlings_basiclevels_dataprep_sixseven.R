# this script aggregates the seedlings basiclevels at the recording-level, and adds some relevant statistics.
# it runs over the all_basiclevel_feather files that are made by concatenating 
# each file's annotations lab internally
# for the 6/7 paper, we only care about month 6/7
# so at the end of this script we write out the subset of all_basiclevel that's month 6/7, 
# and the aggregated version of just the month six/seven subset (and the overall agg file for other purposes)


#first let's get our libraries and our functions

library(tidyverse)
library(feather)
library(forcats)
library(entropy)
library(blabr)
options(tibble.width = Inf)
options(tibble.print_max = 200, tibble.print_min = 100)

all_basiclevel_home_data <- get_all_basiclevel()%>%
  add_chi_noun_onset()

summary(all_basiclevel_home_data, maxsum=50)
levels(all_basiclevel_home_data$speaker)

#should produce no rows.
all_basiclevel_home_data%>%
  filter(nchar(as.character(speaker))!=3)

subset(all_basiclevel_home_data, is.na(utterance_type))
 

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

  
num_experimentwords <- all_basiclevel_home_data %>%
  count_experimentwords()

         
# MOT and FAT count -------------------------------------------------------
six_to_seventeen_home_FAT_MOT_count <- all_basiclevel_home_data %>%
  count_mot_fat()

# utterance type count ----------------------------------------------------
six_to_seventeen_home_utt_count <-  all_basiclevel_home_data %>%
  count_utterance()


# object present count ----------------------------------------------------
six_to_seventeen_home_op <- all_basiclevel_home_data %>%
  count_object_present()

six_to_seventeen_home_op_exp <- all_basiclevel_home_data %>%
  object_present_exp()

  
# device and toy use count ------------------------------------------------
six_to_seventeen_home_device_count <-  all_basiclevel_home_data %>%
  count_device_and_toy()


# few versions of kid talk info -------------------------------------------

#chi tokens
six_to_seventeen_home_chi_count <-  all_basiclevel_home_data %>%
  count_chi()

#chi types
six_to_seventeen_home_chi_type_count <-  all_basiclevel_home_data %>%
  count_chi_types()

# noun production onset age
six_to_seventeen_home_noun_chi_onset <- all_basiclevel_home_data %>%
  chi_noun_onset()

# finally, big aggregation of our little datasets -------------------------

all_basiclevel_home_data_agg <- big_aggregate(all_basiclevel_home_data)

summary(all_basiclevel_home_data_agg, maxsum=50)


#overall agg feather
write_feather(all_basiclevel_home_data_agg, "data/all_basiclevel_home_data_agg_feather12-11-17")

# two feathers, agg, and not-agg, six/sev month only
write_feather(all_basiclevel_home_data_agg %>% filter(month=="06" | month=="07"), "data/sixsevmonth_basiclevel_home_data_agg_feather")
write_feather(all_basiclevel_home_data%>% filter(month=="06" | month=="07"), "data/sixsevmonth_basiclevel_home_data_feather")
