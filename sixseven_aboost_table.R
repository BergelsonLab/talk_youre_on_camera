

source("sixseven_data_aggregation.R")

aboost_mean <- sixseven_spreadAV %>% 
  dplyr::select(subj, month, a_total_min, a_tot_nosilsk, a_tot_nosil,v_total_min,
                a_numtypes, v_numtypes, a_numtokens, v_numtokens, a_numspeakers, v_numspeakers,a_MOT, v_MOT, a_FAT, v_FAT, a_d, v_d, a_q, v_q, a_i, v_i, a_s, v_s, a_r, v_r, a_n, v_n, a_y_op, v_y_op) %>% 
  mutate_if(is.numeric, funs(na_if(., 0)))  %>% 
  group_by(month) %>% 
  summarise(
    aboost_min = mean(a_total_min/v_total_min),
    aboost_awakemin = mean(a_tot_nosil/v_total_min),
    aboost_types =mean(a_numtypes/v_numtypes, na.rm=T),
    aboost_tokens = mean(a_numtokens/v_numtokens, na.rm=T),
    aboost_speakers = mean(a_numspeakers/v_numspeakers, na.rm=T),
    aboost_MOT = mean(a_MOT/v_MOT, na.rm=T),
    aboost_FAT = mean(a_FAT/v_FAT, na.rm=T),
    aboost_d = mean(a_d/v_d, na.rm=T),
    aboost_q = mean(a_q/v_q, na.rm=T),
    aboost_i = mean(a_i/v_i, na.rm=T),
    aboost_s = mean(a_s/v_s, na.rm=T),
    aboost_r = mean(a_r/v_r, na.rm=T),
    aboost_n = mean(a_n/v_n, na.rm=T),
    aboost_op = mean(a_y_op/v_y_op, na.rm=T),
    comp = "mean_aboost") %>% 
  mutate_if(is.numeric, funs(round(., 2)))
# sd of a_boost, removing NAs
aboost_sd<- sixseven_spreadAV %>% 
  dplyr::select(subj, month, a_total_min, a_tot_nosilsk, a_tot_nosil,v_total_min,
                a_numtypes, v_numtypes, a_numtokens, v_numtokens, a_numspeakers, v_numspeakers,a_MOT, v_MOT, a_FAT, v_FAT, a_d, v_d, a_q, v_q, a_i, v_i, a_s, v_s, a_r, v_r, a_n, v_n, a_y_op, v_y_op) %>% 
  mutate_if(is.numeric, funs(na_if(., 0)))  %>% 
  group_by(month) %>% 
  summarise(
    aboost_min = sd(a_total_min/v_total_min),
    aboost_awakemin = sd(a_tot_nosil/v_total_min),
    aboost_types =sd(a_numtypes/v_numtypes, na.rm=T),
    aboost_tokens = sd(a_numtokens/v_numtokens, na.rm=T),
    aboost_speakers = sd(a_numspeakers/v_numspeakers, na.rm=T),
    aboost_MOT = sd(a_MOT/v_MOT, na.rm=T),
    aboost_FAT = sd(a_FAT/v_FAT, na.rm=T),
    aboost_d = sd(a_d/v_d, na.rm=T),
    aboost_q = sd(a_q/v_q, na.rm=T),
    aboost_i = sd(a_i/v_i, na.rm=T),
    aboost_s = sd(a_s/v_s, na.rm=T),
    aboost_r = sd(a_r/v_r, na.rm=T),
    aboost_n = sd(a_n/v_n, na.rm=T),
    aboost_op = sd(a_y_op/v_y_op, na.rm=T),
    comp = "sd_aboost") %>% 
  mutate_if(is.numeric, funs(round(., 2)))
#need to combine mean and sd into a nice single table; ask Shannon/Andrei
aboost_mean %>% as.data.frame()
aboost_sd %>% as.data.frame()

# ___________________________

# Creating one data frame
# aboost_mean_trial <- aboost_mean
# aboost_sd_trial <- aboost_sd
# colnames(aboost_mean_trial) <- paste(colnames(aboost_mean_trial), "mean", sep = "_")
# colnames(aboost_mean_trial)[1] <- "month"
# colnames(aboost_sd_trial) <- paste(colnames(aboost_sd_trial), "sd", sep = "_")
# colnames(aboost_sd_trial)[1] <- "month"
# 
# aboost <- merge(aboost_mean_trial,aboost_sd_trial)
# 
# ??aboost$aboost_min <- c((aboost%>% filter(month== "06"))$aboost_min_mean,(aboost%>% filter(month== "06"))$aboost_min_sd)
# meansd(aboost_test$aboost_min)
# TO DO: Get rid of comp columns


# ------- THIS TABLE WORKS, BUT IT COULD PROBABLY BE MORE STREAMLINED ---------
# Formatting function
meansd <- function(mean,sd) {
  mean <- mean
  sd <- sd
  step2 <- c(mean," (",sd,")")
  result <- paste(step2, sep="", collapse="") 
  return(result)
}

aboost_min <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_min,(aboost_sd %>% filter(month== "06"))$aboost_min),
                meansd((aboost_mean %>% filter(month== "07"))$aboost_min,(aboost_sd %>% filter(month== "07"))$aboost_min))

aboost_awakemin <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_awakemin,(aboost_sd %>% filter(month== "06"))$aboost_awakemin),
                     meansd((aboost_mean %>% filter(month== "07"))$aboost_awakemin,(aboost_sd %>% filter(month== "07"))$aboost_awakemin))

# TO DO: FIX FORMULAS TO CALL CORRECT VALUES
aboost_types <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_min,(aboost_sd %>% filter(month== "06"))$aboost_min),
                  meansd((aboost_mean %>% filter(month== "07"))$aboost_min,(aboost_sd %>% filter(month== "07"))$aboost_min))

aboost_tokens <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_min,(aboost_sd %>% filter(month== "06"))$aboost_min),
                   meansd((aboost_mean %>% filter(month== "07"))$aboost_min,(aboost_sd %>% filter(month== "07"))$aboost_min))

aboost_speakers <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_min,(aboost_sd %>% filter(month== "06"))$aboost_min),
                     meansd((aboost_mean %>% filter(month== "07"))$aboost_min,(aboost_sd %>% filter(month== "07"))$aboost_min))

aboost_MOT <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_min,(aboost_sd %>% filter(month== "06"))$aboost_min),
                meansd((aboost_mean %>% filter(month== "07"))$aboost_min,(aboost_sd %>% filter(month== "07"))$aboost_min))

aboost_FAT <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_min,(aboost_sd %>% filter(month== "06"))$aboost_min),
                meansd((aboost_mean %>% filter(month== "07"))$aboost_min,(aboost_sd %>% filter(month== "07"))$aboost_min))

aboost_d <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_min,(aboost_sd %>% filter(month== "06"))$aboost_min),
              meansd((aboost_mean %>% filter(month== "07"))$aboost_min,(aboost_sd %>% filter(month== "07"))$aboost_min))

aboost_q <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_min,(aboost_sd %>% filter(month== "06"))$aboost_min),
              meansd((aboost_mean %>% filter(month== "07"))$aboost_min,(aboost_sd %>% filter(month== "07"))$aboost_min))

aboost_i <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_min,(aboost_sd %>% filter(month== "06"))$aboost_min),
              meansd((aboost_mean %>% filter(month== "07"))$aboost_min,(aboost_sd %>% filter(month== "07"))$aboost_min))

aboost_s <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_min,(aboost_sd %>% filter(month== "06"))$aboost_min),
              meansd((aboost_mean %>% filter(month== "07"))$aboost_min,(aboost_sd %>% filter(month== "07"))$aboost_min))

aboost_r <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_min,(aboost_sd %>% filter(month== "06"))$aboost_min),
              meansd((aboost_mean %>% filter(month== "07"))$aboost_min,(aboost_sd %>% filter(month== "07"))$aboost_min))

aboost_n <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_min,(aboost_sd %>% filter(month== "06"))$aboost_min),
              meansd((aboost_mean %>% filter(month== "07"))$aboost_min,(aboost_sd %>% filter(month== "07"))$aboost_min))

aboost_op <- c(meansd((aboost_mean %>% filter(month== "06"))$aboost_min,(aboost_sd %>% filter(month== "06"))$aboost_min),
               meansd((aboost_mean %>% filter(month== "07"))$aboost_min,(aboost_sd %>% filter(month== "07"))$aboost_min))


# Table prep
Month = c("06","07")

aboost_table_data <- data.frame(Month,aboost_min,aboost_awakemin,aboost_types,aboost_tokens,aboost_speakers,aboost_MOT,aboost_FAT,aboost_d,
                                aboost_q,aboost_i,aboost_s,aboost_r,aboost_n,aboost_op)

aboost_col_names = c("Month","Minutes","Awake minutes","Word types","Word tokens","Speakers","Mother","Father","Declaratives","Questions","Imperatives",
                     "Singing","Reading","Short-phrase","Object presence")

kable(aboost_table_data, col.names = aboost_col_names,
         format = "markdown", padding = 2)

# FOR RMARKDOWN
# ```{r aboost_table, echo = F, warning = F, fig.caption="Table XX: Audio boost table caption"}
# source("sixseven_aboost_table.R")
# aboost_table
#```
