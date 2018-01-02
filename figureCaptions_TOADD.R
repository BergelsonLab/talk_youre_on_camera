#Figure numbers are as of 01/02/18

#figure 1
```{r gr-derived-counts-67-diff, fig.cap = cap, echo = F, warning = F, fig.height=5}
gr_countvals_long_collapsed
cap <- "Raw count of variables across audio (indicated by solid borders) and video (dashed borders) recordings. Variables, in order: object presence, nouns from mother and father, utterance type, noun-types and tokens, and count of unique speakers."
```

#figure 2
```{r gr-derived-normcounts-diff, fig.cap = cap, echo = F, warning = F, fig.height = 4.5}
gr_countvals_long_norm_collapsed
cap <- "Normalized variable counts across audio (indicated by solid borders) and video (dashed borders) recordings. Normed counts are calculated as the proportion of raw counts over total non-silent recording time (in minutes). Variables, in order: object presence, nouns from mother and father, utterance type, noun-types and tokens, and count of unique speakers."
```

#figure 3
```{r gr-derived-normcounts-corr, fig.cap = cap, echo = F,  fig.height= 3, warning = F}
gr_count_cor_VA_facetmonth_norm_collapsed
cap <- "Normalized count correlations between audio vs. video recordings. Each point indicates the correlation coefficient after calculating nouns per minute of recording for each child, averaged across months 6 & 7. Variables in order: object presence, nouns from mother and father, utterance type, noun-types and tokens, and unique speakers."
```

#figure 4
```{r gr-ut-count-collapsed, fig.cap = cap, echo = F,  fig.height = 3, fig.width = 3, warning = F}
gr_ut_count_collapsed
cap <- "Utterance type proportions across audio (indicated by solid borders) and video (dashed borders) recordings. Proportions were calculated as the number of each noun types over total noun tokens."
```

#figure 5
```{r top-100-logspace, fig.cap = cap, echo = F, warning = F, message= F, fig.width = 9, fig.height = 7}
top100_logspace_av_graph
cap <- "Log-scaled counts of the top 100 words in audio and video recordings. Each node represents the averaged count, across all participants in both months, of an unique noun-token. Words that occured zero times in one recording-type are not shown."
``` 

#figure 6
```{r top100-corr-rectype, fig.cap = cap,echo = F, fig.height = 2, fig.width = 2}
gr_top100_avspread_collapsed
cap <- "Correlations of the top 100 words in audio vs. video recordings. Each node represents the count of a unique word that occurred in both audio and video recordings, averaged across all participants in both months."
```

#figure 7
```{r top10noun-freq, fig.cap = cap, echo = F, warning = F, fig.height = 3, fig.width = 7, out.extra=''}
top10_graph_collapsed
cap<-"Top 10 words by recording type. Each node represents the frequency count of a top audio or video word over both months (x axis) and count of participants who were exposed to the word in either or both months (y axis)."
#overall_top10
```  