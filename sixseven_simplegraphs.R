#with month collapsed, and just counts and normed counts


#graph of diffs in countvals
gr_countvals_long_collapsed <- ggplot(countvals_long_collapsed, 
                                      aes(fill = count_meas, linetype =audio_video, meas_type_fig, countval))+
  stat_summary(fun.y=mean, geom="bar", position = "dodge", aes(linetype=audio_video), color = "black")+
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", position=position_dodge(width=.9), 
               aes(group = interaction(count_meas,audio_video)))+
  facet_wrap(meas_type_fig~audio_video, nrow=3, ncol=4, scales = "free")+
  theme(axis.text.x=element_blank(),
           axis.ticks.x=element_blank())+
  scale_fill_discrete(name="Count\nMeasures",
                      breaks=c("y_op", "MOT", "FAT", "d","q","n","s","r","i","numtypes","numtokens","numspeakers"),
                      labels=c("op","mother","father","declarative","question","short phrase","singing","reading","imperative",
                               "Ntypes","Ntokens","Nspeakers"))+
  xlab("Measure Type")+ylab("Count")+
  scale_linetype(name = "")
#graph of diffs in countvals, normed
gr_countvals_long_norm_collapsed <- ggplot(countvals_long_norm_collapsed, 
                                           aes(fill = norm_meas, linetype =audio_video, audio_video, normval))+
  stat_summary(fun.y=mean, geom="bar", position = "dodge", aes(linetype=audio_video),color = "black")+
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", position=position_dodge(width=.9), 
               aes(group = interaction(norm_meas,audio_video)))+
  facet_wrap(~meas_type_fig, ncol = 2, scales = "free_y")+
  scale_fill_discrete(name="Normed Count\nMeasures",
                      breaks=c("y_op", "MOT", "FAT", "d","q","n","s","r","i","numtypes","numtokens","numspeakers"),
                      labels=c("op","mother","father","declarative","question","short phrase","singing","reading","imperative",
                               "Ntypes","Ntokens","Nspeakers"))+
  xlab("Measure Type")+ylab("Normed Count")+
  scale_linetype(name = "")
#
# 
# #corr in count space, not sure we need this at all
# gr_count_cor_VA_facetmonth_collapsed<-ggplot(countvals_long_collapsed%>% 
#                                      spread(audio_video, countval), aes(audio, video, color = count_meas, shape = meas_type))+
#   geom_jitter(width = .1, height = .1)+
#   facet_wrap(~count_meas, scales = "free", nrow=2)+
#   stat_smooth(color = "red", fill = "black", method = "rlm")+theme_bw(base_size=10)+
#   guides(colour = "none")+
#   scale_shape_discrete(solid = F)+ggtitle("count correlations, AV")
# 
measlabs <- c(y_op="op", MOT = "mother", FAT = "father", n = "short phrase",
              d = "declarative",q = "question", r = "reading", s= "singing", i = "imperative",
              numtypes = "# of noun-types", numtokens = "# of noun-tokens", numspeakers = "# of speakers")

gr_count_cor_VA_facetmonth_norm_collapsed<-ggplot(countvals_long_norm_collapsed%>% 
  spread(audio_video, normval), aes(audio, video, color = norm_meas))+
  geom_point(shape=1)+
  facet_wrap(~norm_meas, scales = "free", nrow=2, 
             labeller = labeller(norm_meas = measlabs),
             strip.position = "right")+
  stat_smooth(color = "red", fill = "black", method = "rlm")+
  theme_bw(base_size=10)+
  guides(colour = "none")+
  xlab("normed audio count") + ylab ("normed video count")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3))


#corr between overall top100 a vs. v
gr_top100_avspread_collapsed <- ggplot(top100av_spread, aes(audio, video))+
  geom_point(shape=2)+
  stat_smooth(method = rlm, fill = "blue")+
  theme_bw(base_size=14)+
  xlab("normed audio count") + ylab ("normed video count")

#top100 graph, log space
#plot in draft# same as in full paper, can take out of other one when cleaning
top100_logspace_av_graph <- ggplot(top100av_spread, aes(audio+.1, video+.1, color = (audio==0 |video==0)))+
  geom_label_repel(aes(label=object),label.size = .10,
                   label.padding = unit(0.1, "lines"),
                   segment.color = "grey90", segment.alpha = .75,force=2)+
  theme_bw(base_size=18)+
  guides(colour=F)+scale_x_log10()+scale_y_log10()+
  xlab("log(audio count+.1)")+ylab("log(video count+.1)")

#plot in draft
#top 10 word fig
top10_graph_collapsed <- ggplot(overall_top10, 
  aes(n, nfams, label = object, shape = audio_video, colour = object, linetype = audio_video))+
  geom_point(size = 2)+
  geom_label_repel(point.padding = .5, box.padding = .4, label.size = .1)+
  theme_bw(base_size=18)+
  scale_shape_discrete(solid=F, name = "")+
  guides(color=F)+
  xlab("word count")+ylab("number of families")

# gr_ut_count_norm_collapsed <- ggplot(countvals_long_norm_collapsed %>% filter(meas_type=="utt"), 
#                                      aes(fill = fct_reorder(norm_meas, normval), linetype =audio_video, audio_video, normval))+
#   stat_summary(fun.y=mean, geom="bar", position = "stack", aes(linetype=audio_video),color = "black")
# 

gr_ut_count_collapsed <- ggplot( countvals_long_collapsed %>% filter(meas_type=="utt"),
       aes(audio_video, countval, fill = fct_reorder(count_meas, countval), linetype =audio_video))+
  stat_summary(fun.y=mean, geom="bar", position = "fill", aes(linetype=audio_video),color = "black")+
  scale_x_discrete(breaks=NULL)+
  scale_fill_discrete(name=NULL,
                      breaks=c("r","s","i","n","q","d"),
                      labels=c("reading","singing","imperative","short phrase", "question","declarative"))+
  scale_linetype(name = "")
#

