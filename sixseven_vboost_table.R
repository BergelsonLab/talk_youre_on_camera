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
                   "Imperatives","Singing","Reading","Short phrases","Object co-presence","comp_mean")

# Formatting function
meansd <- function(mean,sd) {
  mean <- mean
  sd <- sd
  step2 <- c(mean," (",sd,")")
  result <- paste(step2, sep="", collapse="") 
  return(result)
}


# Table data frame with proper formatting
vboost_table <- vboost %>%
  rowwise() %>% 
  mutate(mean_sd = meansd(Mean,SD)) %>%
  dplyr::select(Measure,mean_sd)

vboost_table_data <- vboost_table[-nrow(vboost_table),]
vboost_col_names = c("Measure", "Mean (SD)")

# Table

vboost_table <- kable(vboost_table_data, col.names = vboost_col_names)
