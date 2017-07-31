#misc six seven graphs of all our vars
source("sixseven_data_aggregation.R")
#ggplot(clantime, aes(total_min,SubjectNumber))+geom_point()+theme_bw(base_size=8)
#The 12 Derived Counts####
#numtypes
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, numtokens, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#numtokens
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, numtypes, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#FAT huge diff, maybe 10fold?
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, FAT, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#MOT: big diff but not 10fold
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, MOT, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#numspeakers
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, numspeakers, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

# d big diff but not 10fold
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, i, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#q 
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, q, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")
#s
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, s, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")
#r
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, r, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")
#n
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, n, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")
#i
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, i, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, y_op, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")


#The 11 Derived Proportions##########
#type token: 15% higher in video
# this needs to be done with either a MATTR or vocD measure (see clan)
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, type_token_ratio, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#prop_mom: higher for video by roughly 15%
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, prop_mom, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#prop_dad: comparably low in both;~15%
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, prop_dad, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#propd: 10% more in audio (looks sig dif)
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, propd, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#propi: same
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, propi, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#propn: same
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, propn, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#propq: more in vid
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, propq, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#propr: very similar in both
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, propr, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#props: very similar in both
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, props, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")


#prop_op: higher for video by roughly 10%
ggplot(sixseven_basiclevel_home_data_agg, 
       aes(month, prop_op, color=audio_video))+
  geom_violin()+
  geom_point(position = position_jitterdodge(dodge.width=.9, jitter.width = .1, jitter.height = 0), shape = 21)+
  stat_summary(fun.data=mean_cl_boot, position = position_dodge(width=.9), aes(group=audio_video), color="black")

#actual comparison across audio/video numerically

#correlation graphs####
ggplot(sixseven_spreadAV,
       aes(a_numtypes, v_numtypes, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)

ggplot(sixseven_spreadAV,
       aes(a_numtokens, v_numtokens, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)

ggplot(sixseven_spreadAV,
       aes(a_FAT, v_FAT, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)

ggplot(sixseven_spreadAV,
       aes(a_MOT, v_MOT, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)
ggplot(sixseven_spreadAV,
       aes(a_numspeakers, v_numspeakers, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)


ggplot(sixseven_spreadAV,
       aes(a_d, v_d, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)

ggplot(sixseven_spreadAV,
       aes(a_q, v_q, color=month, fill = month))+
  geom_point()+
  stat_smooth(method="rlm")+facet_wrap(~month)
#
ggplot(sixseven_spreadAV,
       aes(a_i, v_i, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)

ggplot(sixseven_spreadAV,
       aes(a_n, v_n, color=month, fill = month))+
  geom_point()+
  stat_smooth(method="rlm")+facet_wrap(~month)

ggplot(sixseven_spreadAV,
       aes(a_r, v_r, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)

ggplot(sixseven_spreadAV,
       aes(a_s, v_s, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)


#prop a vs. v cors, facet by month

ggplot(sixseven_spreadAV,
       aes(a_prop_op, v_prop_op, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)

#corr
ggplot(sixseven_spreadAV,
       aes(a_prop_mom, v_prop_mom, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)
#sparse
ggplot(sixseven_spreadAV,
       aes(a_prop_dad, v_prop_dad, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)

ggplot(sixseven_spreadAV,
       aes(a_propd, v_propd, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)

ggplot(sixseven_spreadAV,
       aes(a_propq, v_propq, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)

ggplot(sixseven_spreadAV,
       aes(a_props, v_props, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)
#corr
ggplot(sixseven_spreadAV,
       aes(a_propi, v_propi, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)

ggplot(sixseven_spreadAV,
       aes(a_propn, v_propn, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)

ggplot(sixseven_spreadAV,
       aes(a_propr, v_propr, color=month, fill = month))+
  geom_point()+
  stat_smooth()+facet_wrap(~month)

ggplot(sixseven_spreadAV,
       aes(a_type_token_ratio, v_type_token_ratio, color=month, fill = month))+
  geom_point()+
  stat_smooth(method="rlm")+facet_wrap(~month)

#cor six vs. seven count vars, faceted by av
ggplot(sixseven_spreadmonth, aes(six_numspeakers, sev_numspeakers))+geom_jitter(width=.1, height=0)+stat_smooth()+facet_wrap(~audio_video, scales = "free", nrow  =2)
ggplot(sixseven_spreadmonth, aes(six_numtypes, sev_numtypes))+geom_point()+stat_smooth()+facet_wrap(~audio_video, scales = "free", nrow  =2)
ggplot(sixseven_spreadmonth, aes(six_FAT, sev_FAT))+geom_point()+stat_smooth()+facet_wrap(~audio_video, scales = "free", nrow  =2)
ggplot(sixseven_spreadmonth, aes(six_MOT, sev_MOT))+geom_point()+stat_smooth()+facet_wrap(~audio_video, scales = "free", nrow  =2)
ggplot(sixseven_spreadmonth, aes(six_d, sev_d))+geom_point()+stat_smooth()+facet_wrap(~audio_video, scales = "free", nrow  =2)
ggplot(sixseven_spreadmonth, aes(six_q, sev_q))+geom_point()+stat_smooth()+facet_wrap(~audio_video, scales = "free", nrow  =2)
ggplot(sixseven_spreadmonth, aes(six_i, sev_i))+geom_point()+stat_smooth()+facet_wrap(~audio_video, scales = "free", nrow  =2)
ggplot(sixseven_spreadmonth, aes(six_r, sev_r))+geom_point()+stat_smooth()+facet_wrap(~audio_video, scales = "free", nrow  =2)
ggplot(sixseven_spreadmonth, aes(six_s, sev_s))+geom_point()+stat_smooth()+facet_wrap(~audio_video, scales = "free", nrow  =2)
ggplot(sixseven_spreadmonth, aes(six_n, sev_n))+geom_point()+stat_smooth()+facet_wrap(~audio_video, scales = "free", nrow  =2)
ggplot(sixseven_spreadmonth, aes(six_y_op, sev_y_op))+geom_point()+stat_smooth()+facet_wrap(~audio_video, scales = "free", nrow  =2)



#distribution of ut, op, and speakers, proportionally and by count####
##stacked bargraphs

#ut stacked proportionally
ggplot(sixseven_basiclevel_home_data, 
       aes(audio_video, fill = utterance_type))+
  geom_bar(position = "fill")+
  facet_wrap(~month)

#ut stacked by count
ggplot(sixseven_basiclevel_home_data, 
       aes(audio_video, fill = utterance_type))+
  geom_bar()+
  facet_wrap(~month)


#op stacked proportionally
ggplot(sixseven_basiclevel_home_data, 
       aes(audio_video, fill = object_present))+
  geom_bar(position = "fill")+
  facet_wrap(~month)

#op stacked by count
ggplot(sixseven_basiclevel_home_data, 
       aes(audio_video, fill = object_present))+
  geom_bar()+
  facet_wrap(~month)

#speaker stacked proportionally
ggplot(sixseven_basiclevel_home_data, 
       aes(audio_video, fill = speaker))+
  geom_bar(position = "fill")+
  facet_wrap(~month)

#speaker stacked by count
ggplot(sixseven_basiclevel_home_data, 
       aes(audio_video, fill = speaker))+
  geom_bar()+
  facet_wrap(~month)