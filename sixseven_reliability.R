library(tidyverse)
library(irr)
videorel6 <- read_csv("data/video_06_reliability.csv") %>% filter(new_utt_type!="o")
audiorel6 <- read_csv("data/audio_06_reliability.csv") %>% filter(new_utt_type!="o")
videorel7 <- read_csv("data/video_reliability_07.csv") %>% filter(new_utt_type!="o")
audiorel7 <- read_csv("data/audio_reliability_07.csv") %>% filter(new_utt_type!="o")

videorel <- bind_rows(videorel6, videorel7)
audiorel <- bind_rows(audiorel6, audiorel7)

videorel%>% 
  mutate(utrel = ifelse(orig_utt_type==new_utt_type, T, F),
         oprel = ifelse(orig_present==new_present, T, F),
         orig_present = factor(orig_present),
         new_present = factor(new_present),
         orig_utt_type = factor(orig_utt_type),
         new_utt_type = factor(new_utt_type))%>%
  summary()

kappavidUT<- kappa2(videorel[,c("orig_utt_type","new_utt_type")])
kappavidOP<-kappa2(videorel[,c("orig_present","new_present")])
agreevidUT<-agree(videorel[,c("orig_utt_type","new_utt_type")])
agreevidOP<-agree(videorel[,c("orig_present","new_present")])

audiorel%>% 
  mutate(utrel = ifelse(orig_utt_type==new_utt_type, T, F),
         oprel = ifelse(orig_present==new_present, T, F),
         orig_present = factor(orig_present),
         new_present = factor(new_present),
         orig_utt_type = factor(orig_utt_type),
         new_utt_type = factor(new_utt_type))%>%
  #filter(utrel==F | oprel==F) %>% #write_csv(.,"audio_rel_withagreement.csv")
  summary(maxsum=50)


kappaaudUT<- kappa2(audiorel[,c("orig_utt_type","new_utt_type")])
kappaaudOP<-kappa2(audiorel[,c("orig_present","new_present")])
agreeaudUT<-agree(audiorel[,c("orig_utt_type","new_utt_type")])
agreeaudOP<-agree(audiorel[,c("orig_present","new_present")])

meankappaOP <- round(mean(c(kappaaudOP$value,kappavidOP$value)),2)#may want to round
meankappaUT <- round(mean(c(kappaaudUT$value,kappavidUT$value)),2)
meanagreeOP <- round(mean(c(agreeaudOP$value, agreevidOP$value),2))
meanagreeUT <- round(mean(c(agreeaudUT$value, agreevidUT$value),2))