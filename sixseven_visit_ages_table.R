# creates table of recordings ages for months 6-7

library(dplyr)
library(knitr)

visit_ages <- read.csv("data/sixseven_visit_ages.csv")

visit_ages_6 <- visit_ages %>%
  filter(month == "6")

visit_ages_7 <- visit_ages %>%
  filter(month == "7")

# MM;DD function
mmdd <- function(mean,sd) {
  mos <- floor(mean/30.42)
  days <- round(mean-(mos*30.42),digits = 0)
  step2 <- c("M=",mos,";",days,", SD=",round(sd,digits = 1)," days")
  result <- paste(step2, sep="", collapse="") 
  return(result)
}

# 6mo lab visit
sixmo_lab = mmdd(mean(visit_ages_6$lab_age_days),sd(visit_ages_6$lab_age_days))

# 6mo audio
sixmo_audio = mmdd(mean(visit_ages_6$audio_age_days),sd(visit_ages_6$audio_age_days))

# 6mo video
sixmo_video = mmdd(mean(visit_ages_6$video_age_days),sd(visit_ages_6$video_age_days))

# 7mo audio
sevenmo_audio = mmdd(mean(visit_ages_7$audio_age_days),sd(visit_ages_7$audio_age_days))

# 7mo video
sevenmo_video = mmdd(mean(visit_ages_7$video_age_days),sd(visit_ages_7$video_age_days))


# Table prep
Month = c("6 months","7 months")

Video_recordings = c(sixmo_video,sevenmo_video)

Audio_recordings = c(sixmo_audio,sevenmo_audio)

Lab_visits = c(sixmo_lab,"NA")

col_names = c("","Video recordings","Audio recordings","In-lab visits")

ages_table_data <- data.frame(Month, Video_recordings, Audio_recordings, Lab_visits)


# Table
sixseven_visit_ages_table <- kable(ages_table_data, col.names = col_names,
                                   format = "markdown", padding = 2)

# FOR RMARKDOWN
# ```{r recording-ages-table, comment=F, message=F, hide = T, warning = F, echo = F}
# source("sixseven_visit_ages_table.R")
# sixseven_visit_ages_table
# ```