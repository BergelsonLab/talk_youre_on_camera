source("sixseven_data_aggregation.R")
source("sixseven_simplestats.R")
library(knitr)

vboost_mean_new <- vboost_mean_collapsed
vboost_sd_new <- vboost_sd_collapsed

colnames(vboost_mean_new) <- paste(colnames(vboost_mean_new), "mean", sep = "_")
colnames(vboost_sd_new) <- paste(colnames(vboost_sd_new), "sd", sep = "_")

vboost_mean_new <- vboost_mean_new %>% t()
vboost_sd_new <- vboost_sd_new %>% t()

vboost <- data.frame(cbind(vboost_mean_new,vboost_sd_new))

vboost <- setNames(cbind(rownames(vboost), vboost, row.names = NULL), 
                   c("Measure", "Mean", "SD"))

vboost <- vboost %>%
  transform(Mean = as.numeric(as.character(Mean)),
         SD = as.numeric(as.character(SD)))

vboost$Measure = c("Minutes","Awake minutes","Types","Tokens","Speakers","Mother","Father","Declaratives","Questions",
                   "Imperatives","Singing","Reading","Short phrases","Object presence","comp_mean")

# Formatting function
meansd <- function(mean,sd) {
  mean <- mean
  sd <- sd
  step2 <- c(mean," (",sd,")")
  result <- paste(step2, sep="", collapse="") 
  return(result)
}


# Table data frame with proper formatting
vboost_table_data <- vboost %>%
  rowwise() %>% 
  mutate(mean_sd = meansd(Mean,SD)) %>%
  dplyr::select(Measure,mean_sd) %>% 
  filter(Measure !="comp_mean") %>% 
  rename(`Video-fraction Mean(SD)`= mean_sd)

sixseven_spreadAV_normmin_collapsed %>% 
  summarise(mean(vid_total_min/aud_tot_nosil))
  

countvals_normed_vboost_table <- countvals_long_norm_collapsed %>% 
  group_by(audio_video, norm_meas) %>% 
  summarise(mnv = mean(normval, na.rm=T)) %>% 
  spread(audio_video, mnv) %>% 
  mutate(norm_inflation = video/audio) %>% 
  dplyr::select(norm_meas, norm_inflation) %>% 
  mutate_if(is.numeric, funs(round(.,1))) %>% 
  mutate(norm_meas = fct_recode(norm_meas,
                                "Object presence" = "y_op",
                                "Mother" = "MOT",
                                "Father" = "FAT",
                                "Declaratives" = "d",
                                "Questions" = "q",
                                "Short phrases" = "n",
                                "Singing" = "s",
                                "Reading" = "r",
                                "Imperatives" = "i",
                                "Types" = "numtypes",
                                "Tokens" = "numtokens",
                                "Speakers" = "numspeakers")) %>%
  rename("Measure"=norm_meas,
         "Inflation (normed)"=norm_inflation) %>% 
  right_join(vboost_table_data)
