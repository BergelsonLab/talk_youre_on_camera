library(dplyr)

setwd("/Users/sd314/Desktop/")
video_rec_ages <- read.csv("video_recording_ages_subnums.csv")
audio_rec_ages <- read.csv("audio_recording_ages_lena.csv")

audio_rec_ages_6_7 <- audio_rec_ages %>%
  filter(month %in% c("6","7"))

recording_ages_6_7 <- merge(audio_rec_ages_6_7,video_rec_ages)

recording_ages_6 <- recording_ages_6_7 %>%
  filter(month == "6")

recording_ages_7 <- recording_ages_6_7 %>%
  filter(month == "7")

mmdd <- function(mean,sd) {
  mos <- floor(mean/30.42)
  days <- round(mean-(mos*30.42),digits = 0)
    step2 <- c("M=",mos,";",days,", SD=",round(sd,digits = 1)," days")
    result <- paste(step2, sep="", collapse="") 
    return(result)
}

# 6mo audio
mmdd(mean(recording_ages_6$audio_age_days),sd(recording_ages_6$audio_age_days))

# 6mo video
mmdd(mean(recording_ages_6$video_age_days),sd(recording_ages_6$video_age_days))

# 7mo audio
mmdd(mean(recording_ages_7$audio_age_days),sd(recording_ages_7$audio_age_days))

# 7mo video
mmdd(mean(recording_ages_7$video_age_days),sd(recording_ages_7$video_age_days))