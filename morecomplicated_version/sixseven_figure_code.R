#graph of diffs in propvals
gr_propvals_long <- ggplot(propvals_long, aes(fill = prop_meas, linetype =audio_video, month, propval))+
  stat_summary(fun.y=mean, geom="bar", position = "dodge", aes(linetype=audio_video),color = "black")+
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", position=position_dodge(width=.9), 
               aes(group = interaction(prop_meas,month,audio_video)))+
  facet_wrap(~meas_type, nrow = 2, scales = "free_y")+ggtitle("prop variables")
#graph of diffs in countvals
gr_countvals_long <- ggplot(countvals_long, aes(fill = count_meas, linetype =audio_video, month, countval))+
  stat_summary(fun.y=mean, geom="bar", position = "dodge", aes(linetype=audio_video), color = "black")+
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", position=position_dodge(width=.9), 
               aes(group = interaction(count_meas,month,audio_video)))+
  facet_wrap(meas_type~audio_video, scales = "free_y", ncol=2)+ggtitle("count variables")
#graph of diffs in countvals, normed
gr_countvals_long_norm <- ggplot(countvals_long_norm, aes(fill = norm_meas, linetype =audio_video, month, normval))+
  stat_summary(fun.y=mean, geom="bar", position = "dodge", aes(linetype=audio_video),color = "black")+
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", position=position_dodge(width=.9), 
               aes(group = interaction(norm_meas,month,audio_video)))+
  facet_wrap(~meas_type, ncol = 2, scales = "free_y")+ggtitle("normcount variables")

#graph of cor  V vs. A, by month
gr_prop_cor_VA_facetmonth <- ggplot(propvals_long%>% 
  spread(audio_video, propval), aes(audio, video, color = prop_meas, shape = meas_type))+
  geom_jitter(width = .1, height = .1)+
  facet_wrap(month~prop_meas, scales = "free", nrow=2)+
  #facet_grid(month~meas_type+count_meas, scales = "free")+
  stat_smooth(color = "red", fill = "black", method = "lm")+theme_bw(base_size=10)+
  guides(colour = "none")+
  scale_shape_discrete(solid = F)

gr_prop_cor_67_facetav <- ggplot(propvals_long%>% 
  spread(month, propval), aes(`06`, `07`, color = prop_meas, shape = meas_type))+
  geom_jitter(width = .1, height = .1)+
  facet_wrap(audio_video~prop_meas, scales = "free",nrow= 2)+
  #facet_grid(audio_video~meas_type+count_meas, scales = "free")+
  stat_smooth(color = "red", fill = "black", method = "lm")+theme_bw(base_size=10)+
  guides(colour = "none")+
  scale_shape_discrete(solid = F)

gr_count_cor_VA_facetmonth<-ggplot(countvals_long%>% 
  spread(audio_video, countval), aes(audio, video, color = count_meas, shape = meas_type))+
  geom_jitter(width = .1, height = .1)+
  facet_wrap(month~count_meas, scales = "free", nrow=2)+
  #facet_grid(month~meas_type+count_meas, scales = "free")+
  stat_smooth(color = "red", fill = "black", method = "lm")+theme_bw(base_size=10)+
  guides(colour = "none")+
    scale_shape_discrete(solid = F)
  
gr_count_cor_67_facetav<- ggplot(countvals_long%>% 
  spread(month, countval), aes(`06`, `07`, color = count_meas, shape = meas_type))+
  geom_jitter(width = .1, height = .1)+
  facet_wrap(audio_video~count_meas, scales = "free",nrow= 2)+
  #facet_grid(audio_video~meas_type+count_meas, scales = "free")+
  stat_smooth(color = "red", fill = "black", method = "lm")+theme_bw(base_size=10)+
  guides(colour = "none")+
  scale_shape_discrete(solid = F)

gr_count_cor_VA_facetmonth_norm<-ggplot(countvals_long_norm%>% 
  spread(audio_video, normval), aes(audio, video, color = norm_meas, shape = meas_type))+
  #geom_jitter(width = .1, height = .1)+
  geom_point()+
  facet_wrap(month~norm_meas, scales = "free", nrow=2)+
  #facet_grid(month~meas_type+count_meas, scales = "free")+
  stat_smooth(color = "red", fill = "black", method = "lm")+theme_bw(base_size=10)+
  guides(colour = "none")+
  scale_shape_discrete(solid = F)

gr_count_cor_67_facetav_norm <- ggplot(countvals_long_norm%>% 
   spread(month, normval), aes(`06`, `07`, color = norm_meas, shape = meas_type))+
  #geom_jitter(width = .1, height = .1)+
  geom_point()+
  facet_wrap(audio_video~norm_meas, scales = "free",nrow= 2)+
  #facet_grid(audio_video~meas_type+count_meas, scales = "free")+
  stat_smooth(color = "red", fill = "black", method = "lm")+theme_bw(base_size=10)+
  guides(colour = "none")+
  scale_shape_discrete(solid = F)

#cor top100 by month a vs. v
#plots in paper draft
gr_top100_avspread <- ggplot(top100av67_avspread, aes(audio, video))+facet_wrap(~month)+
  geom_point(shape=2)+
  stat_smooth(method = rlm, fill = "blue")+
  theme_bw(base_size=14)+ggtitle("Freq. of Top 100 Words, Audio vs. Video Counts")

gr_top100_monthspread <- ggplot(top100_monthspread, aes(six, seven))+facet_wrap(~audio_video, scales = "free")+
  geom_point(shape =1) + 
  stat_smooth(method = rlm, fill = "blue")+
  theme_bw(base_size=14)+ggtitle("Freq. of Top 100 Words, 6 vs 7 mo. Counts")

#plot in paper draft
zipf_av_67 <- ggplot(tally_month_av, aes(n, fill = month))+geom_histogram()+
  facet_grid(month~audio_video,scales = "free")+
  scale_x_log10()+scale_y_log10()+ggtitle("Rank by Frequency, A, V, 6, 7")

#top100 graph, log space
#plot in draft
top100_logspace_av_graph <- ggplot(top100av_spread, aes(audio+.1, video+.1, color = (audio==0 |video==0)))+
  geom_label_repel(aes(label=object),label.size = .10,
                   label.padding = unit(0.1, "lines"),
                   segment.color = "grey90")+
  theme_bw(base_size=18)+
  guides(colour=F)+scale_x_log10()+scale_y_log10()+
  ggtitle("Top 100 words,log space")

#plot in draft
top10_graph <- ggplot(overall_month_top10, aes(n, nfams, label = object, colour = audio_video, fill = object))+
  facet_wrap(~month)+
  geom_label_repel(segment.color = "grey50")+
  theme_bw(base_size=18)+
  scale_colour_manual(values = c("pink","black"))+
  ggtitle("Top 10 words by recording type and month")+
  guides(fill=F)#+#theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
  #theme(legend.key=element_rect(fill='springgreen'))


# #ggplot(top100av67_avspread, aes(audio+.1, video+.1, color = (audio==0 |video==0)))+
# ggplot(top100av67_avspread, aes(audio, video, color = (audio==0 |video==0)))+
#   geom_label_repel(aes(label=object),label.size = .10,
#   label.padding = unit(0.1, "lines"), size = 3,
#   point.padding = unit(1.6, 'lines'),
#   segment.color = '#cccccc',
#   segment.alpha =.5,
#   force=.5, fill = "white")+
# #  nudge_x = ifelse(top100av67_avspread$video == 0, 10,
# #                   ifelse(top100av67_avspread$audio ==0, -10, 0)),
# #  nudge_y = ifelse(top100av67_avspread$video == 0, -40,
# #                   ifelse(top100av67_avspread$audio ==0, 20, 0)))+
#       #arrow = arrow(length = unit(0.02, 'npc')))+
#   theme_bw(base_size=14)+
#   guides(color=F)+facet_wrap(~month, nrow=1)+
#   geom_point()#+scale_x_log10()+scale_y_log10()
