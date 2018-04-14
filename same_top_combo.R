#combining samehour and tophour new dataframes,graphs, analyses
library(blabr)
source("sixseven_data_aggregation.R")
source("sixseven_simplestats.R")
#these four feathers are all you need for all the new same/top figs (plus stuff from main agg script)
# they are created by the tophour_sixseven_dataprep&agg.R & samehour_sixseven_dataprep&agg.R scripts)
#the specific stats for each sub-part (same and top) are in those separate scripts.

TOPHOURsixseven_basiclevel_home_data_agg<- read_feather("data/TOPHOURsixseven_basiclevel_home_data_agg_feather_04_13_18")
TOPHOURsixseven_basiclevel_home_data<- read_feather("data/TOPHOURsixseven_basiclevel_home_data_04_13_18")
TOPHOUR_marker <- read_feather("data/TOPHOUR_marker_04_13_18")
SAMEHOURsixseven_basiclevel_home_data_agg<- read_feather("data/SAMEHOURsixseven_basiclevel_home_data_agg_feather_04_13_18")
SAMEHOURsixseven_basiclevel_home_data<- read_feather("data/SAMEHOURsixseven_basiclevel_home_data_04_13_18")
SAMEHOUR_marker <- read_feather("data/SAMEHOUR_marker_04_13_18")

TOPHOURcountvals_long <- TOPHOURsixseven_basiclevel_home_data_agg %>% 
  dplyr::select(month, audio_video, subj, y_op, MOT, FAT, d, q, n, s, r, i, numtypes, numtokens, numspeakers) %>% 
  group_by(month, audio_video, subj) %>% 
  gather(count_meas, countval, y_op:numspeakers) %>% 
  mutate(count_meas=factor(count_meas),
         count_meas = fct_relevel(count_meas,
                                  c("y_op", "MOT", "FAT", "d", "q", "n", "s", "r", "i", "numtypes", "numtokens", "numspeakers")),
         meas_type = fct_collapse(count_meas,
                                  speaker = c("MOT","FAT"),
                                  speaker_num = c("numspeakers"),
                                  utt = c("d","q","n","s","r","i"),
                                  quant = c("numtypes","numtokens"),
                                  op = "y_op"))


TOPHOURcountvals_long_collapsed <- TOPHOURcountvals_long %>% 
  ungroup() %>% 
  dplyr::select(-month) %>% 
  group_by(subj, count_meas, audio_video, meas_type) %>% 
  summarise_all(mean, na.rm=T) %>% 
  mutate(meas_type_fig = fct_recode(meas_type, Nspeakers="speaker_num"))#forgraph


SAMEHOURcountvals_long <- SAMEHOURsixseven_basiclevel_home_data_agg %>% 
  dplyr::select(month, audio_video, subj, y_op, MOT, FAT, d, q, n, s, r, i, numtypes, numtokens, numspeakers) %>% 
  group_by(month, audio_video, subj) %>% 
  gather(count_meas, countval, y_op:numspeakers) %>% 
  mutate(count_meas=factor(count_meas),
         count_meas = fct_relevel(count_meas,
                                  c("y_op", "MOT", "FAT", "d", "q", "n", "s", "r", "i", "numtypes", "numtokens", "numspeakers")),
         meas_type = fct_collapse(count_meas,
                                  speaker = c("MOT","FAT"),
                                  speaker_num = c("numspeakers"),
                                  utt = c("d","q","n","s","r","i"),
                                  quant = c("numtypes","numtokens"),
                                  op = "y_op"))


SAMEHOURcountvals_long_collapsed <- SAMEHOURcountvals_long %>% 
  ungroup() %>% 
  dplyr::select(-month) %>% 
  group_by(subj, count_meas, audio_video, meas_type) %>% 
  summarise_all(mean, na.rm=T) %>% 
  mutate(meas_type_fig = fct_recode(meas_type, Nspeakers="speaker_num"))#forgraph

timeslices_countvals_long_collapsed <- SAMEHOURcountvals_long_collapsed %>% 
  mutate(timeslice = ifelse(audio_video=="video","video","same")) %>% 
  bind_rows(TOPHOURcountvals_long_collapsed %>% filter(audio_video=="audio") %>% mutate(timeslice ="top")) %>% 
  bind_rows(countvals_long_collapsed %>% filter(audio_video=="audio") %>% mutate(timeslice ="audio_day")) %>% 
  mutate(hr_day = factor(ifelse(timeslice=="audio_day", "day","hour")),
         timeslice = factor(timeslice))

# top10 & top100 all timeslices ----------------------------------------------------

TOPHOURoverall_top10 <-TOPHOURsixseven_basiclevel_home_data%>%
  group_by(audio_video, object)%>%
  summarise(n = n(),
            nfams = n_distinct(subj)) %>% 
  top_n(10,n) %>% 
  arrange(audio_video, -n)

SAMEHOURoverall_top10 <-SAMEHOURsixseven_basiclevel_home_data%>%
  group_by(audio_video, object)%>%
  summarise(n = n(),
            nfams = n_distinct(subj)) %>% 
  top_n(10,n) %>% 
  arrange(audio_video, -n)


timeslices_overall_top10<- SAMEHOURoverall_top10 %>%  
  mutate(timeslice = ifelse(audio_video=="video","video_hr","audio_samehr")) %>% 
  bind_rows(TOPHOURoverall_top10 %>% filter(audio_video=="audio") %>% mutate(timeslice ="audio_tophr")) %>% 
  bind_rows(overall_top10 %>% filter(audio_video=="audio") %>% mutate(timeslice="audio_day"))

timeslices_top100av <- SAMEHOURsixseven_basiclevel_home_data%>%
  mutate(timeslice = ifelse(audio_video=="video","video_hr","audio_samehr")) %>% 
  bind_rows(TOPHOURsixseven_basiclevel_home_data %>% filter(audio_video=="audio") %>% mutate(timeslice ="audio_tophr")) %>% 
  bind_rows(sixseven_basiclevel_home_data %>% filter(audio_video=="audio") %>% mutate(timeslice="audio_day")) %>% 
  mutate(timeslice = as.factor(timeslice)) %>% 
  group_by(timeslice, object)%>%
  tally()%>%
  top_n(100,n)

timeslices_top100av_spread <- timeslices_top100av %>% 
  spread(timeslice, n,fill = 0) %>% 
  group_by(object) %>% 
  mutate(aud_vid_sum = sum(audio_day,audio_samehr,audio_tophr, video_hr, na.rm=T)) %>% 
  arrange(-aud_vid_sum)

# df to plot hour onsets and offsets on one graph -------------------------------------------------------------------
combo_sametopmarker <- SAMEHOUR_marker %>% 
  left_join(TOPHOUR_marker) %>% gather(timetype,startstop, ms_start_samehour:ms_end_tophour)
combo_sametopmarker_wide <- SAMEHOUR_marker %>% 
  left_join(TOPHOUR_marker) 

#filter(combo_sametopmarker, audiorec_startime > video_start_time & timetype=="ms_start_samehour")
#overlap between tophour and samehour, n=15, but actually n=12 bc three were constrained to be that way bc 
#video started before audio
top_same_overlappers <- combo_sametopmarker_wide %>% 
  mutate(overlap_SHTH = ifelse((ms_start_samehour>ms_start_tophour &
                                  ms_start_samehour<(ms_start_tophour+60*60*1000))|
                                 (ms_start_tophour > ms_start_samehour &
                                    ms_start_tophour<(ms_start_samehour+60*60*1000)),T, F)) %>% 
  filter(overlap_SHTH==T)#nb: 25_07 & 12_06 had manually adjusted start times to hour1 bc SH was before audio recordings

# combo_graphs ------------------------------------------------------------
same_top_day_graph <- ggplot(combo_sametopmarker %>% filter(timetype %in% c("ms_start_samehour","ms_start_tophour")), 
       aes(startstop,SubjectNumber, color = timetype, linetype = timetype, size= SubjectNumber %in% c("25_07","12_06","37_06")))+
  geom_segment(aes(yend = SubjectNumber, xend = startstop+(60*60*1000)))+
  theme_bw()+
  scale_x_continuous(labels=function(startstop)round(startstop/(60*60*1000),1))+
  scale_linetype_manual(values = c(1,3),name="",
                        breaks=c("ms_start_samehour", "ms_start_tophour"),
                        labels=c("Video Hr.", "Top Audio Hr."))+
  scale_color_manual(values = c("red","black"),name="",
                     breaks=c("ms_start_samehour", "ms_start_tophour"),
                     labels=c("Video Hr.", "Top Audio Hr."))+
  scale_size_manual(values = c(1.5,1))+
  guides(size=F, linetype= guide_legend("Hour Type"),color= guide_legend("Hour Type"))+
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+
  labs(x="Timing of Video Hour & Top Audio Hour in Daylong Audio Recording", y="Individual Recordings")


count_meas_fig <- ggplot(timeslices_countvals_long_collapsed, 
       aes(fill = count_meas, linetype =timeslice, timeslice, countval))+
  stat_summary(fun.y=mean, geom="bar", position = "dodge", aes(linetype=timeslice),color = "black", size = .4)+
  stat_summary(fun.data=mean_cl_boot, geom="linerange", position=position_dodge(width=.9), 
               aes(group = interaction(count_meas,timeslice)),size = .7, linetype=1)+
  facet_wrap(hr_day~meas_type_fig, nrow=2,scales = "free")+
  scale_fill_discrete(name="Count\nMeasures",
                      breaks=c("y_op", "MOT", "FAT", "d","q","n","s","r","i","numtypes","numtokens","numspeakers"),
                      labels=c("op","mother","father","declarative","question","short phrase","singing","reading","imperative",
                               "Ntypes","Ntokens","Nspeakers"))+
  xlab("Measure Type")+ylab("Count")+
  scale_linetype(name = "")

prop_ut_fig <- ggplot(timeslices_countvals_long_collapsed %>% filter(meas_type=="utt"),
       aes(timeslice, countval, fill = fct_reorder(count_meas, countval), linetype =timeslice))+
  stat_summary(fun.y=mean, geom="bar", position = "fill", aes(linetype=timeslice),color = "black")+
  scale_x_discrete(breaks=NULL)+
  guides(linetype=F)+
  scale_fill_discrete(name=NULL,
                      breaks=c("s","r","i","n","q","d"),
                      labels=c("singing","reading","imperative","short phrase", "question","declarative"))+
  scale_linetype(name = "")


# ggplot(timeslices_top100av_spread, aes(audio_tophr+.1, video_hr+.1, color = (audio_tophr==0 |video_hr==0)))+
#   geom_label_repel(aes(label=object),label.size = .10,
#                    label.padding = unit(0.1, "lines"),
#                    segment.color = "grey90", segment.alpha = .75,force=2)+
#   theme_bw(base_size=18)+
#   guides(colour=F)+scale_x_log10()+scale_y_log10()+
#   xlab("log(audio count+.1)")+ylab("log(video count+.1)")

top10_all4_fig <- ggplot(timeslices_overall_top10, 
       aes(n, nfams, label = object, shape = timeslice, colour = object, linetype = audio_video))+
  geom_point(size = 2, color = "black")+
  geom_label_repel(point.padding = .5, box.padding = .4, label.size = .1)+
  theme_bw(base_size=18)+
  scale_shape_discrete(solid=F, name = "", guide=F)+
  guides(color=F)+
  xlab("word count")+ylab("number of families")+
 facet_grid(~timeslice)

ggplot(timeslices_overall_top10, aes(object, fill=timeslice, linetype = timeslice, color=timeslice))+geom_bar(stat="count", color="black")


# # tophour specific graphs -------------------------------------------------
# ggplot(TOPHOURcountvals_long_collapsed, 
#        aes(fill = count_meas, linetype =audio_video, meas_type_fig, countval))+
#   stat_summary(fun.y=mean, geom="bar", position = "dodge", aes(linetype=audio_video), color = "black")+
#   stat_summary(fun.data=mean_cl_boot, geom="pointrange", position=position_dodge(width=.9), 
#                aes(group = interaction(count_meas,audio_video)))+
#   facet_wrap(meas_type_fig~audio_video, nrow=3, ncol=4, scales = "free")+
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())+
#   scale_fill_discrete(name="Count\nMeasures",
#                       breaks=c("y_op", "MOT", "FAT", "d","q","n","s","r","i","numtypes","numtokens","numspeakers"),
#                       labels=c("op","mother","father","declarative","question","short phrase","singing","reading","imperative",
#                                "Ntypes","Ntokens","Nspeakers"))+
#   xlab("Measure Type")+ylab("Top Hour Count")+
#   scale_linetype(name = "")
# 
# ggplot(TOPHOURcountvals_long_collapsed, 
#        aes(fill = count_meas, linetype =audio_video, audio_video, countval))+
#   stat_summary(fun.y=mean, geom="bar", position = "dodge", aes(linetype=audio_video),color = "black")+
#   stat_summary(fun.data=mean_cl_boot, geom="pointrange", position=position_dodge(width=.9), 
#                aes(group = interaction(count_meas,audio_video)))+
#   facet_wrap(~meas_type_fig, ncol = 2, scales = "free_y")+
#   scale_fill_discrete(name="Normed Count\nMeasures",
#                       breaks=c("y_op", "MOT", "FAT", "d","q","n","s","r","i","numtypes","numtokens","numspeakers"),
#                       labels=c("op","mother","father","declarative","question","short phrase","singing","reading","imperative",
#                                "Ntypes","Ntokens","Nspeakers"))+
#   xlab("Measure Type")+ylab("Top Hour Count")+
#   scale_linetype(name = "")
# 
# ggplot( TOPHOURcountvals_long_collapsed %>% filter(meas_type=="utt"),
#         aes(audio_video, countval, fill = fct_reorder(count_meas, countval), linetype =audio_video))+
#   stat_summary(fun.y=mean, geom="bar", position = "fill", aes(linetype=audio_video),color = "black")+
#   scale_x_discrete(breaks=NULL)+
#   scale_fill_discrete(name=NULL,
#                       breaks=c("r","s","i","n","q","d"),
#                       labels=c("reading","singing","imperative","short phrase", "question","declarative"))+
#   scale_linetype(name = "")
# #
# 
# ggplot(TOPHOURcountvals_long_collapsed%>%
#          spread(audio_video, countval), aes(audio, video, color = count_meas, shape = meas_type))+
#   geom_jitter(width = .1, height = .1)+
#   facet_wrap(~count_meas, scales = "free", nrow=2)+
#   stat_smooth(color = "red", fill = "black", method = "rlm")+theme_bw(base_size=10)+
#   guides(colour = "none")+
#   scale_shape_discrete(solid = F)+ggtitle("count correlations, AV")
# 
# # same hour specific graphs -----------------------------------------------
# 
# ggplot(SAMEHOURcountvals_long_collapsed, 
#        aes(fill = count_meas, linetype =audio_video, meas_type_fig, countval))+
#   stat_summary(fun.y=mean, geom="bar", position = "dodge", aes(linetype=audio_video), color = "black")+
#   stat_summary(fun.data=mean_cl_boot, geom="pointrange", position=position_dodge(width=.9), 
#                aes(group = interaction(count_meas,audio_video)))+
#   facet_wrap(meas_type_fig~audio_video, nrow=3, ncol=4, scales = "free")+
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())+
#   scale_fill_discrete(name="Count\nMeasures",
#                       breaks=c("y_op", "MOT", "FAT", "d","q","n","s","r","i","numtypes","numtokens","numspeakers"),
#                       labels=c("op","mother","father","declarative","question","short phrase","singing","reading","imperative",
#                                "Ntypes","Ntokens","Nspeakers"))+
#   xlab("Measure Type")+ylab("Count")+
#   scale_linetype(name = "")
# 
# ggplot(SAMEHOURcountvals_long_collapsed, 
#        aes(fill = count_meas, linetype =audio_video, audio_video, countval))+
#   stat_summary(fun.y=mean, geom="bar", position = "dodge", aes(linetype=audio_video),color = "black")+
#   stat_summary(fun.data=mean_cl_boot, geom="pointrange", position=position_dodge(width=.9), 
#                aes(group = interaction(count_meas,audio_video)))+
#   facet_wrap(~meas_type_fig, ncol = 2, scales = "free_y")+
#   scale_fill_discrete(name="Normed Count\nMeasures",
#                       breaks=c("y_op", "MOT", "FAT", "d","q","n","s","r","i","numtypes","numtokens","numspeakers"),
#                       labels=c("op","mother","father","declarative","question","short phrase","singing","reading","imperative",
#                                "Ntypes","Ntokens","Nspeakers"))+
#   xlab("Measure Type")+ylab("Top Hour Count")+
#   scale_linetype(name = "")
# 
# ggplot(SAMEHOURcountvals_long_collapsed %>% filter(meas_type=="utt"),
#        aes(audio_video, countval, fill = fct_reorder(count_meas, countval), linetype =audio_video))+
#   stat_summary(fun.y=mean, geom="bar", position = "fill", aes(linetype=audio_video),color = "black")+
#   scale_x_discrete(breaks=NULL)+
#   scale_fill_discrete(name=NULL,
#                       breaks=c("r","s","i","n","q","d"),
#                       labels=c("reading","singing","imperative","short phrase", "question","declarative"))+
#   scale_linetype(name = "")
# #
# 
# ggplot(SAMEHOURcountvals_long_collapsed%>%
#          spread(audio_video, countval), aes(audio, video, color = count_meas, shape = meas_type))+
#   geom_jitter(width = .1, height = .1)+
#   facet_wrap(~count_meas, scales = "free", nrow=2)+
#   stat_smooth(color = "red", fill = "black", method = "rlm")+theme_bw(base_size=10)+
#   guides(colour = "none")+
#   scale_shape_discrete(solid = F)+ggtitle("count correlations, AV")
