#with month collapsed, and just counts and normed counts


#graph of diffs in countvals
gr_countvals_long_collapsed <- ggplot(countvals_long_collapsed, aes(fill = count_meas, linetype =audio_video, meas_type, countval))+
  stat_summary(fun.y=mean, geom="bar", position = "dodge", aes(linetype=audio_video), color = "black")+
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", position=position_dodge(width=.9), 
               aes(group = interaction(count_meas,audio_video)))+
  facet_wrap(meas_type~audio_video, nrow=3, ncol=4, scales = "free")+ggtitle("count variables")
#graph of diffs in countvals, normed
gr_countvals_long_norm_collapsed <- ggplot(countvals_long_norm_collapsed, aes(fill = norm_meas, linetype =audio_video, audio_video, normval))+
  stat_summary(fun.y=mean, geom="bar", position = "dodge", aes(linetype=audio_video),color = "black")+
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", position=position_dodge(width=.9), 
               aes(group = interaction(norm_meas,audio_video)))+
  facet_wrap(~meas_type, ncol = 2, scales = "free_y")+ggtitle("normcount variables")

#corr in count space, not sure we need this at all
gr_count_cor_VA_facetmonth_collapsed<-ggplot(countvals_long_collapsed%>% 
                                     spread(audio_video, countval), aes(audio, video, color = count_meas, shape = meas_type))+
  geom_jitter(width = .1, height = .1)+
  facet_wrap(~count_meas, scales = "free", nrow=2)+
  stat_smooth(color = "red", fill = "black", method = "rlm")+theme_bw(base_size=10)+
  guides(colour = "none")+
  scale_shape_discrete(solid = F)+ggtitle("count correlations, AV")

gr_count_cor_VA_facetmonth_norm_collapsed<-ggplot(countvals_long_norm_collapsed%>% 
  spread(audio_video, normval), aes(audio, video, color = norm_meas, shape = meas_type))+
  geom_point()+
  facet_wrap(~norm_meas, scales = "free", nrow=2)+
  stat_smooth(color = "red", fill = "black", method = "rlm")+theme_bw(base_size=10)+
  guides(colour = "none")+
  scale_shape_discrete(solid = F)+ggtitle("normcount correlations, AV")

#corr between overall top100 a vs. v
gr_top100_avspread_collapsed <- ggplot(top100av_spread, aes(audio, video))+
  geom_point(shape=2)+
  stat_smooth(method = rlm, fill = "blue")+
  theme_bw(base_size=14)

#zipf av plot in paper draft
zipf_av <- ggplot(tally_av, aes(n, fill = audio_video))+geom_histogram()+
  facet_grid(~audio_video)+
  scale_x_log10()+scale_y_log10()+
  guides(fill=F)

#top100 graph, log space
#plot in draft# same as in full paper, can take out of other one when cleaning
top100_logspace_av_graph <- ggplot(top100av_spread, aes(audio+.1, video+.1, color = (audio==0 |video==0)))+
  geom_label_repel(aes(label=object),label.size = .10,
                   label.padding = unit(0.1, "lines"),
                   segment.color = "grey90", segment.alpha = .75,force=2)+
  theme_bw(base_size=18)+
  guides(colour=F)+scale_x_log10()+scale_y_log10()+
  ggtitle("Top 100 words,log space")

#plot in draft
#top 10 word fig
top10_graph_collapsed <- ggplot(overall_top10, aes(n, nfams, label = object, shape = audio_video, colour = object, linetype = audio_video))+
  #ggplot(overall_top10, aes(n, nfams, label = object, colour = audio_video, fill = object, linetype = audio_video))+
  #geom_label_repel(segment.color = "grey50")+
  geom_point(size = 2)+
  geom_label_repel(point.padding = .5, box.padding = .4, label.size = .1)+
  theme_bw(base_size=18)+
  #scale_colour_manual(values = c("pink","black"))+
 # ggtitle("Top 10 words by recording type")+
  scale_shape_discrete(solid=F)+
  guides(color=F)#+#theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
#theme(legend.key=element_rect(fill='springgreen'))


gr_ut_count_norm_collapsed <- ggplot(countvals_long_norm_collapsed %>% filter(meas_type=="utt"), 
                                     aes(fill = fct_reorder(norm_meas, normval), linetype =audio_video, audio_video, normval))+
  stat_summary(fun.y=mean, geom="bar", position = "stack", aes(linetype=audio_video),color = "black")


gr_ut_count_collapsed <- ggplot( countvals_long_collapsed %>% filter(meas_type=="utt"),
       aes(audio_video, countval, fill = fct_reorder(count_meas, countval), linetype =audio_video))+
  stat_summary(fun.y=mean, geom="bar", position = "fill", aes(linetype=audio_video),color = "black")+
  guides(fill=(guide_legend(title = NULL)))

